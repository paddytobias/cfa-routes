# Orchestration: bring the database up to date using a Google client.

# Items that fail `max_attempts` times are skipped on later runs (see db_note_failures()),
# so one bad address or route can't cost a request every day.

pair_key <- function(event, station) paste(event, station, sep = "|")

# Geocode rows lacking coordinates and save the ones Google could place.
geocode_missing <- function(con, client, table, key, rows, max_attempts = 3L) {
  kind <- paste0("geocode_", table)
  todo <- rows[is.na(rows$lat) & !rows[[key]] %in% db_exhausted(con, kind, max_attempts), ]
  if (nrow(todo) == 0) return(rows)
  placed <- resolve_locations(client, todo)
  db_note_failures(con, kind, placed[[key]][is.na(placed$lat)])
  db_set_coords(con, table, key, placed[!is.na(placed$lat), ])
  rows$lat[match(placed[[key]], rows[[key]])] <- placed$lat
  rows$lng[match(placed[[key]], rows[[key]])] <- placed$lng
  rows
}

# Route the incident/station pairs that have no good route yet, for the newest
# `max_incidents` incidents. Returns the events routed.
route_pending <- function(con, client, max_incidents = 5L, home = "Chewton",
                          speed_factor = 1, with_elevation = TRUE, max_attempts = 3L) {
  stations <- geocode_missing(con, client, "stations", "station", db_read_stations(con), max_attempts)
  geocode_missing(con, client, "incidents", "incident", db_read_incidents(con), max_attempts)
  pairs <- db_missing_pairs(con)
  pairs <- pairs[!pair_key(pairs$event, pairs$station) %in% db_exhausted(con, "route", max_attempts), ]
  events <- head(unique(pairs$event), max_incidents)
  pairs <- pairs[pairs$event %in% events, ]
  if (nrow(pairs) == 0) return(character())
  route_station <- function(station) {
    compare_brigades(client, stations[stations$station == station, ],
                     db_read_incidents(con, pairs$event[pairs$station == station]),
                     home = home, speed_factor = speed_factor, with_elevation = with_elevation)
  }
  results <- do.call(rbind, lapply(unique(pairs$station), route_station))
  db_save_routes(con, results)
  db_note_failures(con, "route", pair_key(results$incident, results$station)[!is.na(results$error)])
  events
}

# Fetch route geometry for stored routes that lack it (Chewton's and the paged
# brigades'), without touching their times or distances. One request per route.
# A route Google can't fetch is skipped with a warning. Returns routes updated.
backfill_polylines <- function(con, client, home = "Chewton", max_routes = 50L, max_attempts = 3L) {
  pairs <- db_pairs_without_polyline(con, home)
  pairs <- head(pairs[!pair_key(pairs$event, pairs$station) %in% db_exhausted(con, "polyline", max_attempts), ], max_routes)
  if (nrow(pairs) == 0) return(0L)
  departures <- match_event_time(from_epoch(pairs$time))
  fetch_one <- function(slat, slng, ilat, ilng, when) {
    tryCatch(parse_route(client$routes(routes_request(list(lat = slat, lng = slng),
                                                       list(lat = ilat, lng = ilng), when)))$polyline,
             error = function(e) { warning(conditionMessage(e), call. = FALSE); NA_character_ })
  }
  pairs$polyline <- unlist(Map(fetch_one, pairs$slat, pairs$slng, pairs$ilat, pairs$ilng, departures))
  found <- pairs[!is.na(pairs$polyline), ]
  db_note_failures(con, "polyline", pair_key(pairs$event, pairs$station)[is.na(pairs$polyline)])
  db_set_polylines(con, found)
  nrow(found)
}
