# Run: Rscript tests/test_db.R   (in-memory SQLite; no network, no API key)
for (f in list.files("R", full.names = TRUE)) source(f)
tzm <- "Australia/Melbourne"
ev_time <- function(x) as.POSIXct(x, tz = tzm)

con <- db_connect(":memory:")
stopifnot(all(c("stations", "incidents", "routes") %in% DBI::dbListTables(con)))

# ---- stations: CSV wins, coordinates kept only while the address is unchanged
st <- data.frame(station = c("Chewton", "Castlemaine"), code = c("CCHEW", "CCMAI"),
                 address = c("a", "b"), lat = NA_real_, lng = NA_real_)
db_upsert_stations(con, st)
db_set_coords(con, "stations", "station", data.frame(station = c("Chewton", "Castlemaine"), lat = c(-37.1, -37.0), lng = c(144.2, 144.3)))
db_upsert_stations(con, st)                                         # re-sync: coords survive
stopifnot(identical(db_read_stations(con)$lat, c(-37.1, -37.0)))
st$address[1] <- "moved"; db_upsert_stations(con, st)               # address changed: coords dropped
stopifnot(is.na(db_read_stations(con)$lat[1]), db_read_stations(con)$lat[2] == -37.0)
db_upsert_stations(con, transform(st, address = c("moved", "b"), lat = c(-37.1, NA), lng = c(144.2, NA)))
stopifnot(db_read_stations(con)$lat[1] == -37.1)

# ---- incidents: existing rows win, idempotent, newest first, times round-trip
alerts <- function(ev, t, turnout = "CCMAI CHARC") data.frame(
  event = ev, time = ev_time(t), primary_brigade = "CMAI", response_code = "STRUC1",
  location_text = paste("JOB", ev), map_ref = "SVC 1 A1", turnout = turnout, address_guess = paste(ev, "ST CASTLEMAINE"),
  stringsAsFactors = FALSE)
stopifnot(db_add_incidents(con, alerts(c("F1", "F2"), c("2026-11-14 20:18:53", "2026-11-21 16:06:05"))) == 2)
stopifnot(db_add_incidents(con, alerts("F1", "2026-01-01 00:00:00")) == 0)     # duplicate ignored, history kept
stopifnot(db_add_incidents(con, NULL) == 0)
inc <- db_read_incidents(con)
stopifnot(identical(inc$incident, c("F2", "F1")), inc$event_time[2] == ev_time("2026-11-14 20:18:53"),
          inc$paged[1] == "CCMAI, CHARC", is.na(inc$lat[1]))
stopifnot(identical(names(inc), c("incident", "address", "lat", "lng", "event_time", "incident_address", "paged")))
db_set_coords(con, "incidents", "event", data.frame(event = c("F1", "F2"), lat = c(-37.06, -37.07), lng = c(144.2, 144.3)))
stopifnot(identical(db_read_incidents(con, "F1")$lat, -37.06))

# ---- routes: only incidents lacking a good route for every station are pending
pairs <- db_missing_pairs(con)
stopifnot(identical(pairs$event, c("F2", "F2", "F1", "F1")), identical(pairs$station, rep(c("Chewton", "Castlemaine"), 2)))
mk_route <- function(ev, station, mins, error = NA_character_) data.frame(
  incident = ev, station = station, departure_time = ev_time("2026-11-28 16:06:05"),
  distance_km = mins / 2, duration_traffic_min = mins, duration_free_flow_min = mins - 1, traffic_delay_min = 1,
  pct_normal = 90, pct_slow = 10, pct_jam = 0, elev_gain_m = NA_real_, elev_loss_m = NA_real_,
  elev_max_m = NA_real_, elev_min_m = NA_real_, max_climb_grade_pct = NA_real_, error = error, stringsAsFactors = FALSE)
res <- rbind(mk_route("F2", "Chewton", 12), mk_route("F2", "Castlemaine", 8),
             mk_route("F1", "Chewton", 7), mk_route("F1", "Castlemaine", NA_real_, "boom"))
db_save_routes(con, res, now = ev_time("2026-11-22 12:00:00"))
pairs <- db_missing_pairs(con)
stopifnot(identical(pairs$event, "F1"), identical(pairs$station, "Castlemaine"))   # only the failed pair is retried
db_save_routes(con, mk_route("F1", "Castlemaine", 9))                 # retry succeeds, replaces the error row
stopifnot(nrow(db_missing_pairs(con)) == 0,
          DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM routes")$n == 4)

# ---- analysis read-back
cmp <- db_read_comparison(con, home = "Chewton")
stopifnot(nrow(cmp) == 4, identical(cmp$incident, c("F2", "F2", "F1", "F1")),
          cmp$scenario[1] == "event Sat 16:06", cmp$paged[1] == "CCMAI, CHARC",
          cmp$incident_address[1] == "JOB F2", is.na(cmp$error[1]))
f2 <- cmp[cmp$incident == "F2", ]
stopifnot(f2$minutes_vs_home[f2$station == "Castlemaine"] == -4, f2$rank[f2$station == "Castlemaine"] == 1,
          identical(f2$station_paged[order(f2$station)], c(TRUE, FALSE)))   # Castlemaine paged, Chewton not
stopifnot(db_read_comparison(con, speed_factor = 0.5)$eta_min[1] == 2 * cmp$eta_min[1])
sm <- summarise_brigades(cmp)
stopifnot(identical(sm$station, c("Castlemaine", "Chewton")), sm$pct_fastest == c(50, 50), sm$pct_paged == c(100, 0))   # each fastest once; Chewton never paged
stopifnot(nrow(db_read_comparison(db_connect(":memory:"))) == 0)

# ---- brigade count: only CFA brigade codes, not units like AFP / CASE1 / P73B
stopifnot(identical(count_brigades(c("CCAMP CCMAI CHARC", "AFP CCAMP CCMAI", "AFPR CASE1 CCAMP CCMAI",
                                     "CHARC CKFLT CLOCK P73B", "BDG384 BOM354", "")), c(3L, 2L, 2L, 3L, 0L, 0L)))
stopifnot(identical(cmp$n_paged, rep(2L, 4)))        # fixture turnout is "CCMAI CHARC"

# ---- foreign keys are enforced
stopifnot(inherits(tryCatch(db_save_routes(con, mk_route("NOPE", "Chewton", 5)), error = identity), "error"))

# ---- persistence across connections
path <- tempfile(fileext = ".sqlite"); c1 <- db_connect(path)
db_upsert_stations(c1, st); invisible(db_add_incidents(c1, alerts("F9", "2026-11-20 10:00:00"))); DBI::dbDisconnect(c1)
c2 <- db_connect(path); stopifnot(db_read_incidents(c2)$incident == "F9"); DBI::dbDisconnect(c2)
cat("db tests passed\n")

# ---- pipeline with a fake Google client ----
calls <- list(geocode = 0, routes = 0)
route_resp <- list(routes = list(list(distanceMeters = 6000, duration = "600s", staticDuration = "540s",
  polyline = list(encodedPolyline = googlePolylines::encode(data.frame(lat = c(0, 0), lon = c(0, 0.05)))),
  travelAdvisory = list(speedReadingIntervals = list()))))
fake <- function(method, url, body = NULL, headers = list()) {
  if (grepl("geocode", url)) {
    calls$geocode <<- calls$geocode + 1
    if (grepl("UNPLACEABLE", curl::curl_unescape(url))) return(list(status = "ZERO_RESULTS", results = list()))
    return(list(status = "OK", results = list(list(geometry = list(location = list(lat = -37 - calls$geocode / 100, lng = 144))))))
  }
  calls$routes <<- calls$routes + 1; route_resp
}
client <- google_client("k", fake)
pc <- db_connect(":memory:")
db_upsert_stations(pc, data.frame(station = c("Chewton", "Castlemaine"), code = c("CCHEW", "CCMAI"), address = c("a", "b")))
a <- alerts(c("F1", "F2", "F3"), c("2026-11-14 20:18:53", "2026-11-21 16:06:05", "2026-11-22 01:00:00"))
a$address_guess[3] <- "UNPLACEABLE"
invisible(db_add_incidents(pc, a))

routed <- suppressWarnings(route_pending(pc, client, max_incidents = 1, with_elevation = FALSE))
stopifnot(identical(routed, "F2"), calls$routes == 2)                 # newest placeable incident only (F3 can't be geocoded)
stopifnot(!anyNA(db_read_stations(pc)$lat), sum(!is.na(db_read_incidents(pc)$lat)) == 2)
g <- calls$geocode
routed <- suppressWarnings(route_pending(pc, client, max_incidents = 5, with_elevation = FALSE))
stopifnot(identical(routed, "F1"), calls$routes == 4)                 # F2 not re-routed
stopifnot(calls$geocode == g + 1)                                     # only the unplaceable address is retried
stopifnot(length(suppressWarnings(route_pending(pc, client, with_elevation = FALSE))) == 0, calls$routes == 4)
cmp <- db_read_comparison(pc)
stopifnot(nrow(cmp) == 4, all(is.na(cmp$error)), all(cmp$distance_km == 6), all(is.na(cmp$elev_gain_m)))
# adding a station routes only that station; existing snapshots are untouched
before <- DBI::dbGetQuery(pc, "SELECT event, station, fetched_at FROM routes ORDER BY event, station")
db_upsert_stations(pc, data.frame(station = "Taradale", code = "CTARA", address = "c"))
routed <- suppressWarnings(route_pending(pc, client, with_elevation = FALSE))
stopifnot(identical(sort(routed), c("F1", "F2")), calls$routes == 6)
after <- DBI::dbGetQuery(pc, "SELECT event, station, fetched_at FROM routes ORDER BY event, station")
stopifnot(nrow(after) == 6, identical(merge(before, after)[, 1:3], before))
cat("pipeline tests passed\n")

# ---- polylines: stored with routes, backfilled for the routes that matter, snapshot untouched
pl <- db_connect(":memory:")
db_upsert_stations(pl, data.frame(station = c("Chewton", "Castlemaine", "Harcourt"), code = c("CCHEW", "CCMAI", "CHARC"),
                                  address = c("a", "b", "c"), lat = c(-37.1, -37.0, -37.2), lng = c(144.2, 144.3, 144.1)))
invisible(db_add_incidents(pl, alerts("F1", "2026-11-14 20:18:53", turnout = "CCMAI")))
db_set_coords(pl, "incidents", "event", data.frame(event = "F1", lat = -37.05, lng = 144.25))
db_save_routes(pl, rbind(mk_route("F1", "Chewton", 7), mk_route("F1", "Castlemaine", 5), mk_route("F1", "Harcourt", 9)))
need <- db_pairs_without_polyline(pl)
stopifnot(identical(sort(need$station), c("Castlemaine", "Chewton")))     # Harcourt wasn't paged and isn't Chewton
stopifnot(nrow(db_read_polylines(pl)) == 0)

route_calls <- 0
poly_resp <- function(...) { route_calls <<- route_calls + 1
  list(routes = list(list(distanceMeters = 999000, duration = "9999s", staticDuration = "9999s",
    polyline = list(encodedPolyline = googlePolylines::encode(data.frame(lat = c(0, 0), lon = c(0, 0.05)))),
    travelAdvisory = list(speedReadingIntervals = list())))) }
n <- backfill_polylines(pl, google_client("k", poly_resp))
stopifnot(n == 2, route_calls == 2, nrow(db_read_polylines(pl)) == 2, nrow(db_pairs_without_polyline(pl)) == 0)
kept <- DBI::dbGetQuery(pl, "SELECT duration_traffic_min, distance_km FROM routes WHERE station = 'Chewton'")
stopifnot(kept$duration_traffic_min == 7, kept$distance_km == 3.5)          # stored times not overwritten by the new response
stopifnot(backfill_polylines(pl, google_client("k", poly_resp)) == 0, route_calls == 2)   # nothing left: no requests

failing <- google_client("k", function(...) stop("quota"))
db_save_routes(pl, mk_route("F1", "Chewton", 7))                            # replaces the row, polyline cleared
w <- NULL; got <- withCallingHandlers(backfill_polylines(pl, failing), warning = function(c) { w <<- conditionMessage(c); invokeRestart("muffleWarning") })
stopifnot(got == 0, grepl("quota", w), nrow(db_pairs_without_polyline(pl)) == 1)   # failure skipped, retried next time

# a database created before polylines existed gains the column
old <- tempfile(fileext = ".sqlite"); oc <- DBI::dbConnect(RSQLite::SQLite(), old)
DBI::dbExecute(oc, "CREATE TABLE routes (event TEXT, station TEXT, error TEXT, fetched_at INTEGER)"); DBI::dbDisconnect(oc)
oc <- db_connect(old); stopifnot("polyline" %in% DBI::dbListFields(oc, "routes")); DBI::dbDisconnect(oc)
cat("polyline tests passed\n")
