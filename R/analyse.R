# Composition of the pure pieces and the client.

elevation_cols <- c("elev_gain_m", "elev_loss_m", "elev_max_m", "elev_min_m", "max_climb_grade_pct")

# With `with_elevation = FALSE` the Elevation API is never called and the
# elevation columns are NA, so the result has the same shape either way.
analyse_route <- function(client, origin, destination, departure_time, samples = 100L,
                          with_elevation = TRUE) {
  route <- parse_route(client$routes(routes_request(origin, destination, departure_time)))
  elevation <- if (with_elevation) {
    parse_elevation(client$elevation(route$polyline, samples))
  } else {
    as.data.frame(setNames(as.list(rep(NA_real_, length(elevation_cols))), elevation_cols))
  }
  cbind(route, elevation)
}

# Function operator: a failing route becomes a row with `error` set, so one bad
# request doesn't discard the rest of the batch, but is never silent.
or_error_row <- function(f) {
  force(f)
  function(...) {
    tryCatch(
      cbind(f(...), error = NA_character_),
      error = function(e) data.frame(error = conditionMessage(e))
    )
  }
}

location_cols <- c("address", "lat", "lng")

require_columns <- function(df, cols, what) {
  missing <- setdiff(cols, names(df))
  if (length(missing)) {
    stop(sprintf("%s needs columns %s; missing %s. (Archive rows must go through incidents_config() first.)",
                 what, paste(cols, collapse = ", "), paste(missing, collapse = ", ")), call. = FALSE)
  }
  df
}

# Geocode rows that have no coordinates. An address Google can't place gets NA
# coordinates and a warning, rather than aborting the whole batch.
resolve_locations <- function(client, locations) {
  require_columns(locations, location_cols, "Locations")
  needs <- is.na(locations$lat) | is.na(locations$lng)
  geocode_one <- function(address) {
    tryCatch(parse_geocode(client$geocode(address)),
             error = function(e) {
               warning(sprintf("Could not geocode '%s': %s", address, conditionMessage(e)), call. = FALSE)
               list(lat = NA_real_, lng = NA_real_)
             })
  }
  geocoded <- lapply(locations$address[needs], geocode_one)
  locations$lat[needs] <- vapply(geocoded, `[[`, 0, "lat")
  locations$lng[needs] <- vapply(geocoded, `[[`, 0, "lng")
  locations
}

# Departure for each incident = its own alert time, moved to the same weekday
# and clock time in the future (Google predicts traffic for that slot).
incident_departures <- function(incidents, from = Sys.time()) {
  require_columns(incidents, c("incident", "event_time"), "Incidents")
  data.frame(
    incident = incidents$incident,
    scenario = format(incidents$event_time, "event %a %H:%M"),
    departure_time = match_event_time(incidents$event_time, from)
  )
}

# One row per station x incident. By default each incident is routed at the time
# of its own alert; pass `departures` (scenario, departure_time) to route every
# incident at each of several fixed times instead.
compare_brigades <- function(client, stations, incidents, departures = NULL,
                             home = "Chewton", speed_factor = 1, samples = 100L,
                             with_elevation = TRUE) {
  require_columns(stations, c("station", location_cols), "Stations")
  require_columns(incidents, c("incident", location_cols), "Incidents")
  if (!is.null(departures)) require_columns(departures, c("scenario", "departure_time"), "Departures")
  stopifnot(is.numeric(stations$lat), is.numeric(stations$lng),
            is.numeric(incidents$lat), is.numeric(incidents$lng))
  rename <- function(df, prefix) {
    df <- df[c(prefix, "lat", "lng")]
    setNames(df, c(prefix, paste0(prefix, "_lat"), paste0(prefix, "_lng")))
  }
  grid <- merge(rename(stations, "station"), rename(incidents, "incident"), by = NULL)
  grid <- if (is.null(departures)) {
    merge(grid, incident_departures(incidents), by = "incident")
  } else {
    merge(grid, departures, by = NULL)
  }
  # Reference columns carried through to the results when the incidents have them.
  reference <- intersect(c("incident_address", "paged"), names(incidents))
  grid <- merge(grid, incidents[c("incident", reference)], by = "incident")
  safe_analyse <- or_error_row(analyse_route)
  rows <- Map(
    function(slat, slng, ilat, ilng, when) {
      safe_analyse(client, list(lat = slat, lng = slng), list(lat = ilat, lng = ilng), when, samples,
                   with_elevation)
    },
    grid$station_lat, grid$station_lng, grid$incident_lat, grid$incident_lng, grid$departure_time
  )
  results <- cbind(grid[c("incident", reference, "scenario", "departure_time", "station")],
                   do.call(rbind, lapply(rows, function(r) r[intersect(result_cols, names(r))] |> pad_cols())))
  add_comparison(results, home, speed_factor)
}

result_cols <- c("distance_km", "duration_traffic_min", "duration_free_flow_min", "traffic_delay_min",
                 "pct_normal", "pct_slow", "pct_jam", "elev_gain_m", "elev_loss_m", "elev_max_m",
                 "elev_min_m", "max_climb_grade_pct", "error", "polyline")

pad_cols <- function(df) {
  df[setdiff(result_cols, names(df))] <- NA
  df[result_cols]
}

# `speed_factor` scales Google's passenger-car times to an appliance
# (e.g. 0.9 = truck is 10% slower). Left at 1 until a figure is agreed.
add_comparison <- function(results, home, speed_factor = 1) {
  results$eta_min <- results$duration_traffic_min / speed_factor
  key <- interaction(results$incident, results$scenario, drop = TRUE)
  home_eta <- ave(ifelse(results$station == home, results$eta_min, NA_real_), key,
                  FUN = function(x) mean(x, na.rm = TRUE))
  results$minutes_vs_home <- results$eta_min - home_eta
  results$rank <- ave(results$eta_min, key, FUN = function(x) rank(x, ties.method = "min", na.last = "keep"))
  results
}
