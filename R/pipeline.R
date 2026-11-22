# Orchestration: bring the database up to date using a Google client.

# Geocode rows lacking coordinates and save the ones Google could place.
geocode_missing <- function(con, client, table, key, rows) {
  todo <- rows[is.na(rows$lat), ]
  if (nrow(todo) == 0) return(rows)
  placed <- resolve_locations(client, todo)
  db_set_coords(con, table, key, placed[!is.na(placed$lat), ])
  rows$lat[match(placed[[key]], rows[[key]])] <- placed$lat
  rows$lng[match(placed[[key]], rows[[key]])] <- placed$lng
  rows
}

# Route the incident/station pairs that have no good route yet, for the newest
# `max_incidents` incidents. Returns the events routed.
route_pending <- function(con, client, max_incidents = 5L, home = "Chewton",
                          speed_factor = 1, with_elevation = TRUE) {
  stations <- geocode_missing(con, client, "stations", "station", db_read_stations(con))
  geocode_missing(con, client, "incidents", "incident", db_read_incidents(con))
  pairs <- db_missing_pairs(con)
  events <- head(unique(pairs$event), max_incidents)
  pairs <- pairs[pairs$event %in% events, ]
  if (nrow(pairs) == 0) return(character())
  route_station <- function(station) {
    compare_brigades(client, stations[stations$station == station, ],
                     db_read_incidents(con, pairs$event[pairs$station == station]),
                     home = home, speed_factor = speed_factor, with_elevation = with_elevation)
  }
  db_save_routes(con, do.call(rbind, lapply(unique(pairs$station), route_station)))
  events
}

# Fetch route geometry for stored routes that lack it (Chewton's and the paged
# brigades'), without touching their times or distances. One request per route.
# A route Google can't fetch is skipped with a warning. Returns routes updated.
backfill_polylines <- function(con, client, home = "Chewton", max_routes = 50L) {
  pairs <- head(db_pairs_without_polyline(con, home), max_routes)
  if (nrow(pairs) == 0) return(0L)
  departures <- match_event_time(from_epoch(pairs$time))
  fetch_one <- function(slat, slng, ilat, ilng, when) {
    tryCatch(parse_route(client$routes(routes_request(list(lat = slat, lng = slng),
                                                       list(lat = ilat, lng = ilng), when)))$polyline,
             error = function(e) { warning(conditionMessage(e), call. = FALSE); NA_character_ })
  }
  pairs$polyline <- unlist(Map(fetch_one, pairs$slat, pairs$slng, pairs$ilat, pairs$ilng, departures))
  found <- pairs[!is.na(pairs$polyline), ]
  db_set_polylines(con, found)
  nrow(found)
}
