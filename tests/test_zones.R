# Run: Rscript tests/test_zones.R   (no network, no API key; in-memory SQLite)
for (f in list.files("R", full.names = TRUE)) source(f)
near <- function(a, b, tol = 1e-6) stopifnot(isTRUE(all.equal(a, b, tolerance = tol)))

# ---- grid: a disc of cells, nearest first, ids stable when the radius changes
g <- make_grid(-37.08, 144.26, radius_km = 3, spacing_km = 1)
stopifnot(nrow(g) == 29, g$cell_id[1] == "r+00_c+00", g$dist_km[1] == 0, all(g$dist_km <= 3), !is.unsorted(g$dist_km))
g5 <- make_grid(-37.08, 144.26, radius_km = 5, spacing_km = 1)
stopifnot(all(g$cell_id %in% g5$cell_id))
near(g5$lat[match(g$cell_id, g5$cell_id)], g$lat); near(g5$lng[match(g$cell_id, g5$cell_id)], g$lng)
near(grid_spacing_km(g), 1, 1e-3)
b <- cell_bounds(g, 1)
near(haversine_m(b$lat1, b$lng1, b$lat2, b$lng1) / 1000, rep(1, nrow(b)), 1e-3)
near(haversine_m(b$lat1, b$lng1, b$lat1, b$lng2) / 1000, rep(1, nrow(b)), 1e-3)

# ---- matrix request and parsing (proto3 JSON omits zero indexes)
dest <- data.frame(lat = c(-37.1, -37.2), lng = c(144.2, 144.3))
req <- matrix_request(list(lat = -37.0, lng = 144.0), dest)
stopifnot(req$routingPreference == "TRAFFIC_UNAWARE", length(req$origins) == 1, length(req$destinations) == 2,
          req$destinations[[2]]$waypoint$location$latLng$longitude == 144.3)
resp <- list(
  list(originIndex = 0L, duration = "600s", distanceMeters = 5000L, condition = "ROUTE_EXISTS", status = list()),
  list(destinationIndex = 2L, condition = "ROUTE_NOT_FOUND", status = list(message = "no road")))
p <- parse_matrix(resp, "Chewton", c("a", "b", "c", "d"))
stopifnot(nrow(p) == 4, identical(p$cell_id[1:2], c("a", "c")))
near(p$duration_s[1], 600); near(p$distance_m[1], 5000)
stopifnot(is.na(p$error[1]), p$error[2] == "no road", is.na(p$duration_s[2]),
          setequal(p$cell_id[3:4], c("b", "d")), all(p$error[3:4] == "Missing from response"))

# ---- ranking
tm <- matrix(c(5, 10, 8,    # cell x: Chewton fastest
               9, 6, 12,    # cell y: Chewton second
               20, 4, 7),   # cell z: Chewton last
             nrow = 3, byrow = TRUE, dimnames = list(c("x", "y", "z"), c("Chewton", "Castlemaine", "Harcourt")))
z <- rank_cells(tm, margin_min = 2)
stopifnot(identical(as.character(z$zone), c("First", "Second", "Third")),
          identical(z$first, c("Chewton", "Castlemaine", "Castlemaine")), identical(z$second, c("Harcourt", "Chewton", "Harcourt")))
near(z$lead_min, c(3, -3, -16)); stopifnot(identical(z$contested, c(FALSE, FALSE, FALSE)))
stopifnot(identical(rank_cells(tm, margin_min = 3)$contested[1:2], c(TRUE, TRUE)))       # |lead| <= margin
near(rank_cells(tm, home_delay_min = 4)$home_eta, c(9, 13, 24))                           # turnout delay: Chewton only
stopifnot(identical(as.character(rank_cells(tm, home_delay_min = 4)$zone)[1], "Second"))
near(rank_cells(tm, speed_factor = 0.5)$first_eta, c(10, 12, 8))                          # everyone twice as slow
stopifnot(identical(as.character(rank_cells(tm, excluded = "Castlemaine")$zone), c("First", "First", "Second"))) # Castlemaine out
stopifnot(identical(as.character(rank_cells(tm, excluded = "Chewton")$zone), rep("No data", 3)))
two_out <- rank_cells(tm, excluded = c("Castlemaine", "Harcourt"))
stopifnot(two_out$zone[3] == "First", is.na(two_out$lead_min[3]))
five <- matrix(c(9, 5, 6, 7, 8,  12, 5, 6, 7, 8), nrow = 2, byrow = TRUE,   # Chewton fifth of five, both cells
               dimnames = list(c("p", "q"), c("Chewton", "A", "B", "C", "D")))
stopifnot(identical(as.character(rank_cells(five)$zone), c("Fourth or later", "Fourth or later")),
          identical(rank_cells(five)$home_rank, c(5L, 5L)))
four <- matrix(c(7, 5, 6, 8), 1, dimnames = list("p", c("Chewton", "A", "B", "C")))   # Chewton third of four
stopifnot(identical(as.character(rank_cells(four)$zone), "Third"))
four[1, "Chewton"] <- 9; stopifnot(identical(as.character(rank_cells(four)$zone), "Fourth or later"))
tie <- matrix(c(5, 5), 1, dimnames = list("t", c("Chewton", "Castlemaine")))
stopifnot(rank_cells(tie)$home_rank == 1L)                                                # a tie counts as first
stopifnot(is.null(rank_cells(tm[0, , drop = FALSE])))
long <- data.frame(cell_id = c("x", "x", "y"), station = c("Chewton", "Harcourt", "Chewton"), duration_s = c(60, 120, NA))
m <- cell_time_matrix(long)
stopifnot(identical(dim(m), c(1L, 2L)), m["x", "Harcourt"] == 2)
stopifnot(identical(as.vector(zone_summary(z)), c(1L, 1L, 1L, 0L)), identical(names(zone_summary(z)), c("First", "Second", "Third", "Fourth or later")))

# ---- display radius
ct <- data.frame(cell_id = c("near", "edge", "far"), lat = c(-37.08, -37.08 - 11 / km_per_degree, -37.08 - 13 / km_per_degree), lng = 144.26)
kept <- within_radius(ct, list(lat = -37.08, lng = 144.26))
stopifnot(identical(kept$cell_id, c("near", "edge")), zone_radius_km == 12)
stopifnot(nrow(within_radius(ct, list(lat = -37.08, lng = 144.26), radius_km = 20)) == 3)

# ---- database and fetch with a fake Google client
con <- db_connect(":memory:")
db_upsert_stations(con, data.frame(station = c("Chewton", "Harcourt", "NoPin"), code = NA_character_,
                                   address = c("a", "b", "c"), lat = c(-37.08, -36.99, NA), lng = c(144.26, 144.26, NA)))
db_add_cells(con, g[1:5, ]); db_add_cells(con, g[1:7, ])                                    # re-adding is harmless
stopifnot(nrow(db_read_cells(con)) == 7)
stored <- db_read_cells(con)$cell_id                                                        # ties in distance sort differently in R and SQLite
pairs <- db_missing_cell_pairs(con)
stopifnot(nrow(pairs) == 14, pairs$cell_id[1] == stored[1])                              # station without coordinates is skipped

calls <- list()
fake <- list(matrix = function(body) {
  calls[[length(calls) + 1]] <<- body
  n <- length(body$destinations)
  lapply(seq_len(n) - 1L, function(i) {
    if (i == 1) list(destinationIndex = 1L, condition = "ROUTE_NOT_FOUND", status = list(message = "off road"))
    else list(destinationIndex = i, duration = "300s", distanceMeters = 2500L, condition = "ROUTE_EXISTS", status = list())
  })
})
nosleep <- function(s) NULL
n <- fetch_cell_times(con, fake, max_elements = 6, chunk = 2, sleep = nosleep)
stopifnot(n == 6, length(calls) == 4)                                                        # 3 cells x 2 stations in chunks of 2
stopifnot(all(vapply(calls, function(b) b$routingPreference == "TRAFFIC_UNAWARE", TRUE)))
saved <- db_read_cell_times(con)
print(table(saved$station))
stopifnot(nrow(saved) == 4, !any(saved$cell_id == stored[2]))                             # the failed cell isn't stored as a time
stopifnot(nrow(db_missing_cell_pairs(con)) == 10)                                            # 14 - 6 fetched + 2 failed
stopifnot(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM failures WHERE kind = 'cell_time'")$n == 2L)
cat("zone tests passed\n")

# ---- moving a station clears what was measured from the old position
con <- db_connect(":memory:")
st <- data.frame(station = c("Chewton", "Harcourt"), code = NA_character_, address = c("a", "b"),
                 lat = c(-37.08, -36.99), lng = c(144.26, 144.26))
stopifnot(length(db_upsert_stations(con, st)) == 0)                                          # first load clears nothing
db_add_cells(con, g[1:2, ])
db_save_cell_times(con, data.frame(cell_id = rep(g$cell_id[1:2], 2), station = rep(c("Chewton", "Harcourt"), each = 2),
                                   duration_s = 60, distance_m = 1000, error = NA_character_))
DBI::dbExecute(con, "INSERT INTO incidents (event, time, lat, lng) VALUES ('E1', 1, -37, 144)")
DBI::dbExecute(con, "INSERT INTO routes (event, station, fetched_at) VALUES ('E1', 'Chewton', 1), ('E1', 'Harcourt', 1)")
db_note_failures(con, "cell_time", c(pair_key(g$cell_id[1], "Chewton"), pair_key(g$cell_id[1], "Harcourt")))
db_note_failures(con, "route", c("E1|Chewton", "E1|Harcourt"))
count <- function(tbl) DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) n FROM %s", tbl))$n

stopifnot(length(db_upsert_stations(con, st)) == 0, count("cell_times") == 4, count("routes") == 2)   # unchanged: nothing cleared
moved <- st; moved$lat[1] <- -37.081
stopifnot(identical(db_upsert_stations(con, moved), "Chewton"))                              # Chewton moved
stopifnot(count("cell_times") == 2, count("routes") == 1, count("failures") == 2,            # only Harcourt's results and failures remain
          all(db_read_cell_times(con)$station == "Harcourt"), DBI::dbGetQuery(con, "SELECT station FROM routes")$station == "Harcourt")
stopifnot(!any(grepl("Chewton", DBI::dbGetQuery(con, "SELECT key FROM failures")$key)))
stopifnot(length(db_upsert_stations(con, moved)) == 0)                                       # re-running with the same position is a no-op

blank <- moved; blank$lat <- blank$lng <- NA_real_                                           # CSV without coordinates keeps the stored ones
stopifnot(length(db_upsert_stations(con, blank)) == 0, db_read_stations(con)$lat[1] == -37.081)
changed_address <- blank; changed_address$address[2] <- "elsewhere"                          # new address drops coordinates: stale too
stopifnot(identical(db_upsert_stations(con, changed_address), "Harcourt"), count("cell_times") == 0, is.na(db_read_stations(con)$lat[2]))
stopifnot(length(db_upsert_stations(con, changed_address)) == 0)                             # still unplaced: nothing further to clear
cat("station move tests passed\n")

# ---- refresh_stations(): clear and refetch only the named stations
con <- db_connect(":memory:")
db_upsert_stations(con, data.frame(station = c("Chewton", "Harcourt"), code = c("CCHEW", "CHARC"), address = c("a", "b"),
                                   lat = c(-37.08, -36.99), lng = c(144.26, 144.26)))
invisible(db_add_incidents(con, data.frame(event = "E1", time = as.POSIXct("2026-11-14 20:18:53", tz = "Australia/Melbourne"),
  primary_brigade = "CMAI", response_code = "STRUC1", location_text = "JOB", map_ref = "x", turnout = "CCHEW CHARC", address_guess = "X ST")))
db_set_coords(con, "incidents", "event", data.frame(event = "E1", lat = -37.05, lng = 144.2))
db_add_cells(con, g[1:3, ])
route_resp <- list(routes = list(list(distanceMeters = 6000, duration = "600s", staticDuration = "540s",
  polyline = list(encodedPolyline = googlePolylines::encode(data.frame(lat = c(0, 0), lon = c(0, 0.05)))),
  travelAdvisory = list(speedReadingIntervals = list()))))
asked <- list(routes = 0, matrix_origins = character())
counting <- list(
  routes = function(body) { asked$routes <<- asked$routes + 1; route_resp },
  matrix = function(body) {
    asked$matrix_origins <<- c(asked$matrix_origins, body$origins[[1]]$waypoint$location$latLng$latitude)
    lapply(seq_along(body$destinations) - 1L, function(i) list(destinationIndex = i, duration = "300s", distanceMeters = 2500L,
                                                               condition = "ROUTE_EXISTS", status = list()))
  })
invisible(route_pending(con, counting, with_elevation = FALSE)); fetch_cell_times(con, counting, sleep = nosleep)
stopifnot(asked$routes == 2, count("cell_times") == 6)
Sys.sleep(1.1)                                                                               # fetched_at has one-second resolution
harcourt_before <- DBI::dbGetQuery(con, "SELECT fetched_at FROM routes WHERE station = 'Harcourt'")$fetched_at
asked <- list(routes = 0, matrix_origins = character())
done <- refresh_stations(con, counting, "Chewton")
stopifnot(identical(done, c(incidents = 1L, grid_pairs = 3L)), asked$routes == 1, identical(asked$matrix_origins, "-37.08"))   # Chewton only
stopifnot(count("cell_times") == 6, count("routes") == 2,
          identical(DBI::dbGetQuery(con, "SELECT fetched_at FROM routes WHERE station = 'Harcourt'")$fetched_at, harcourt_before))
stopifnot(inherits(tryCatch(refresh_stations(con, counting, c("Chewton", "Nowhere")), error = identity), "error"), count("routes") == 2)  # nothing cleared on a typo
asked <- list(routes = 0, matrix_origins = character())
done <- refresh_stations(con, NULL, c("Chewton", "Harcourt"), fetch = FALSE)                  # clear only: no client needed
stopifnot(identical(done, c(incidents = 0L, grid_pairs = 0L)), count("routes") == 0, count("cell_times") == 0, asked$routes == 0)
cat("refresh tests passed\n")
