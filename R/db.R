# SQLite storage. Connections and queries are the impure boundary; the row
# shaping helpers are pure. Times are stored as epoch seconds (timezone-safe)
# and returned in Melbourne time.

schema <- c(
  "CREATE TABLE IF NOT EXISTS stations (
     station TEXT PRIMARY KEY, code TEXT, address TEXT NOT NULL, lat REAL, lng REAL)",
  "CREATE TABLE IF NOT EXISTS incidents (
     event TEXT PRIMARY KEY, time INTEGER NOT NULL, primary_brigade TEXT, response_code TEXT,
     location_text TEXT, map_ref TEXT, turnout TEXT, address_guess TEXT, lat REAL, lng REAL)",
  "CREATE TABLE IF NOT EXISTS routes (
     event TEXT NOT NULL REFERENCES incidents(event), station TEXT NOT NULL REFERENCES stations(station),
     departure_time INTEGER, distance_km REAL, duration_traffic_min REAL, duration_free_flow_min REAL,
     traffic_delay_min REAL, pct_normal REAL, pct_slow REAL, pct_jam REAL,
     elev_gain_m REAL, elev_loss_m REAL, elev_max_m REAL, elev_min_m REAL, max_climb_grade_pct REAL,
     error TEXT, fetched_at INTEGER NOT NULL, polyline TEXT, PRIMARY KEY (event, station))",
  "CREATE TABLE IF NOT EXISTS failures (
     kind TEXT NOT NULL, key TEXT NOT NULL, attempts INTEGER NOT NULL, PRIMARY KEY (kind, key))"
)

route_cols <- c("distance_km", "duration_traffic_min", "duration_free_flow_min", "traffic_delay_min",
                "pct_normal", "pct_slow", "pct_jam", elevation_cols, "error")

to_epoch <- function(t) as.numeric(t)
from_epoch <- function(x, tz = "Australia/Melbourne") .POSIXct(x, tz = tz)

# "CCAMP CCMAI" -> "CCAMP, CCMAI"
format_paged <- function(turnout) {
  vapply(strsplit(turnout, "\\s+"), paste, "", collapse = ", ")
}

# Number of CFA brigades in a turnout. Brigade pager codes are "C" plus four
# letters (CCMAI, CHARC); other codes (AFP, CASE1, P73B, BDG384) are not brigades.
count_brigades <- function(turnout) {
  vapply(strsplit(turnout, "\\s+"), function(codes) sum(grepl("^C[A-Z]{4}$", codes)), integer(1))
}

# Was the station's pager code in the turnout? NA when the station has no code.
was_paged <- function(code, turnout) {
  mapply(function(cd, tn) if (is.na(cd)) NA else cd %in% strsplit(tn, "\\s+")[[1]],
         code, turnout, USE.NAMES = FALSE)
}

db_connect <- function(path = "data/cfa_routes.sqlite") {
  if (path != ":memory:") dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  for (ddl in schema) DBI::dbExecute(con, ddl)
  ensure_column(con, "routes", "polyline", "TEXT")   # databases created before polylines were stored
  con
}

ensure_column <- function(con, table, column, type) {
  if (!column %in% DBI::dbListFields(con, table)) {
    DBI::dbExecute(con, sprintf("ALTER TABLE %s ADD COLUMN %s %s", table, column, type))
  }
  invisible(con)
}

# ---- failed attempts ----------------------------------------------------------

# Google requests that failed, counted per item, so a permanently bad address or
# route stops costing a request on every run. `kind` names the step (geocode_incidents,
# geocode_stations, route, polyline); `key` identifies the item ("event|station" for routes).
db_note_failures <- function(con, kind, keys) {
  if (length(keys) == 0) return(invisible(con))
  DBI::dbExecute(con, "
    INSERT INTO failures (kind, key, attempts) VALUES (?, ?, 1)
    ON CONFLICT(kind, key) DO UPDATE SET attempts = attempts + 1",
    params = list(rep(kind, length(keys)), as.character(keys)))
  invisible(con)
}

# Keys of `kind` that have failed `max_attempts` times and should no longer be tried.
db_exhausted <- function(con, kind, max_attempts) {
  DBI::dbGetQuery(con, "SELECT key FROM failures WHERE kind = ? AND attempts >= ?",
                  params = list(kind, as.integer(max_attempts)))$key
}

# ---- stations ---------------------------------------------------------------

# CSV wins for address/code. Coordinates given in the CSV win; otherwise
# existing ones are kept only while the address is unchanged.
db_upsert_stations <- function(con, stations) {
  for (col in c("code")) if (!col %in% names(stations)) stations[[col]] <- NA_character_
  for (col in c("lat", "lng")) if (!col %in% names(stations)) stations[[col]] <- NA_real_
  invisible(DBI::dbExecute(con, "
    INSERT INTO stations (station, code, address, lat, lng) VALUES (?, ?, ?, ?, ?)
    ON CONFLICT(station) DO UPDATE SET
      code = excluded.code,
      lat = CASE WHEN excluded.lat IS NOT NULL THEN excluded.lat
                 WHEN stations.address = excluded.address THEN stations.lat END,
      lng = CASE WHEN excluded.lng IS NOT NULL THEN excluded.lng
                 WHEN stations.address = excluded.address THEN stations.lng END,
      address = excluded.address",
    params = unname(as.list(stations[c("station", "code", "address", "lat", "lng")]))))
}

db_read_stations <- function(con) {
  DBI::dbGetQuery(con, "SELECT station, code, address, lat, lng FROM stations ORDER BY rowid")
}

# ---- incidents --------------------------------------------------------------

# `alerts` as returned by parse_alerts() plus address_guess. Existing events are
# left untouched, so re-running never rewrites history. Returns rows added.
db_add_incidents <- function(con, alerts) {
  if (is.null(alerts) || nrow(alerts) == 0) return(0L)
  alerts$time <- to_epoch(alerts$time)
  DBI::dbExecute(con, "
    INSERT INTO incidents (event, time, primary_brigade, response_code, location_text, map_ref, turnout, address_guess)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT(event) DO NOTHING",
    params = unname(as.list(alerts[c("event", "time", "primary_brigade", "response_code",
                                     "location_text", "map_ref", "turnout", "address_guess")])))
}

# Incidents in the shape resolve_locations()/compare_brigades() expect, newest first.
db_read_incidents <- function(con, events = NULL) {
  rows <- DBI::dbGetQuery(con, "SELECT * FROM incidents ORDER BY time DESC")
  if (!is.null(events)) rows <- rows[rows$event %in% events, ]
  data.frame(
    incident = rows$event, address = rows$address_guess, lat = rows$lat, lng = rows$lng,
    event_time = from_epoch(rows$time), incident_address = rows$location_text,
    paged = format_paged(rows$turnout), stringsAsFactors = FALSE
  )
}

db_set_coords <- function(con, table, key, locations) {
  stopifnot(table %in% c("stations", "incidents"))
  col <- c(stations = "station", incidents = "event")[[table]]
  invisible(DBI::dbExecute(con, sprintf("UPDATE %s SET lat = ?, lng = ? WHERE %s = ?", table, col),
                           params = unname(list(locations$lat, locations$lng, locations[[key]]))))
}

# ---- routes -----------------------------------------------------------------

# Incident/station pairs (newest incident first) with no successful route yet and
# coordinates for both ends. Failed rows are retried; successful ones are kept as
# the stored snapshot, so adding a station only routes that station.
db_missing_pairs <- function(con) {
  DBI::dbGetQuery(con, "
    SELECT i.event, s.station FROM incidents i CROSS JOIN stations s
    LEFT JOIN routes r ON r.event = i.event AND r.station = s.station AND r.error IS NULL
    WHERE r.event IS NULL AND i.lat IS NOT NULL AND s.lat IS NOT NULL
    ORDER BY i.time DESC, s.rowid")
}

# `results` as returned by compare_brigades().
db_save_routes <- function(con, results, now = Sys.time()) {
  rows <- data.frame(event = results$incident, station = results$station,
                     departure_time = to_epoch(results$departure_time),
                     results[intersect(c(route_cols, "polyline"), names(results))],
                     fetched_at = to_epoch(now), stringsAsFactors = FALSE)
  cols <- names(rows)
  invisible(DBI::dbExecute(con, sprintf("INSERT OR REPLACE INTO routes (%s) VALUES (%s)",
                                        paste(cols, collapse = ", "), paste(rep("?", length(cols)), collapse = ", ")),
                           params = unname(as.list(rows))))
}

# The analysis table: stored routes joined to their incident and station, with
# ETA, rank and minutes-vs-home worked out for the chosen speed factor.
db_read_comparison <- function(con, home = "Chewton", speed_factor = 1) {
  raw <- DBI::dbGetQuery(con, "
    SELECT r.*, i.time AS event_time, i.location_text, i.turnout, s.code AS station_code
    FROM routes r JOIN incidents i ON i.event = r.event JOIN stations s ON s.station = r.station
    ORDER BY i.time DESC, r.station")
  if (nrow(raw) == 0) return(raw)
  event_time <- from_epoch(raw$event_time)
  results <- data.frame(
    incident = raw$event, event_time = event_time, incident_address = raw$location_text, paged = format_paged(raw$turnout),
    n_paged = count_brigades(raw$turnout),
    scenario = format(event_time, "event %a %H:%M"), departure_time = from_epoch(raw$departure_time),
    station = raw$station, station_paged = was_paged(raw$station_code, raw$turnout),
    raw[route_cols], stringsAsFactors = FALSE
  )
  add_comparison(results, home, speed_factor)
}

# ---- polylines (route geometry for the map) -----------------------------------

db_read_polylines <- function(con) {
  DBI::dbGetQuery(con, "SELECT event, station, polyline FROM routes WHERE polyline IS NOT NULL AND error IS NULL")
}

# Stored routes with no geometry yet that matter to the case: Chewton's, and
# those of the brigades actually paged.
db_pairs_without_polyline <- function(con, home = "Chewton") {
  DBI::dbGetQuery(con, "
    SELECT r.event, r.station, s.lat AS slat, s.lng AS slng, i.lat AS ilat, i.lng AS ilng, i.time
    FROM routes r JOIN stations s ON s.station = r.station JOIN incidents i ON i.event = r.event
    WHERE r.polyline IS NULL AND r.error IS NULL
      AND (r.station = ? OR instr(' ' || i.turnout || ' ', ' ' || s.code || ' ') > 0)
    ORDER BY i.time DESC", params = list(home))
}

db_set_polylines <- function(con, rows) {
  invisible(DBI::dbExecute(con, "UPDATE routes SET polyline = ? WHERE event = ? AND station = ?",
                           params = unname(as.list(rows[c("polyline", "event", "station")]))))
}

# One row per station: how often it is fastest, and typical ETA.
summarise_brigades <- function(comparison) {
  ok <- comparison[!is.na(comparison$eta_min), ]
  if (nrow(ok) == 0) stop("No successfully routed incidents to summarise; check the `error` column.", call. = FALSE)
  by_station <- split(ok, ok$station)
  rows <- lapply(names(by_station), function(s) {
    d <- by_station[[s]]
    data.frame(station = s, incidents = nrow(d),
               median_eta_min = median(d$eta_min), mean_eta_min = mean(d$eta_min),
               median_km = median(d$distance_km),
               pct_fastest = 100 * mean(d$rank == 1), pct_paged = 100 * mean(d$station_paged, na.rm = TRUE),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out[order(out$median_eta_min), ]
}
