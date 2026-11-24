# Compare travel times from r5r (routing on OpenStreetMap, no API charges) with the Google
# times already stored in data/cfa_routes.sqlite, to judge whether r5r could replace Google
# for the incident routes and the turnout zones grid. Nothing is written to the database.
#
#   Rscript compare_r5r.R
#
# Setup (one-off): JDK 21 (brew install openjdk@21), osmium (brew install osmium-tool), and the R
# packages r5r and data.table in the project library. They are deliberately not listed in
# DESCRIPTION, so the deployed app and the daily workflow don't install them. The first run
# downloads Victoria's road network (about 250 MB) into osm/ and crops it to the district.
#
# Three variants are measured against Google's free-flow times:
#   defaults     r5r as shipped
#   global scale one speed multiplier, fitted to the incident routes
#   class speeds speeds set from each road's maxspeed tag, else a typical rural limit for its
#                class (class_speeds in R/compare.R, chosen in advance, not fitted)
# Results are printed and saved as CSV files in results/.

Sys.setenv(JAVA_HOME = Sys.getenv("JAVA_HOME", "/opt/homebrew/opt/openjdk@21/libexec/openjdk.jdk/Contents/Home"))
options(java.parameters = "-Xmx4G")
for (pkg in c("r5r", "data.table", "sf")) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Install the R package ", pkg, " first.", call. = FALSE)
}
for (f in list.files("R", full.names = TRUE)) source(f)

home <- "Chewton"
radius_km <- 12                     # the radius the zones map shows
departure <- as.POSIXct("2026-12-01 10:00:00", tz = "Australia/Melbourne")   # no traffic is modelled; r5r just needs a time
pbf_url <- "https://download.geofabrik.de/australia-oceania/australia/victoria-latest.osm.pbf"
district <- "osm/district"
district_bbox <- "143.8,-37.5,144.7,-36.7"   # west,south,east,north: the 25 km grid plus a margin

# ---- road network ------------------------------------------------------------

if (!file.exists(file.path(district, "district.osm.pbf"))) {
  if (!nzchar(Sys.which("osmium"))) stop("Install osmium (brew install osmium-tool) to crop the road data.", call. = FALSE)
  dir.create(district, recursive = TRUE, showWarnings = FALSE)
  state <- "osm/victoria-latest.osm.pbf"
  if (!file.exists(state)) {
    message("Downloading Victoria road data (about 250 MB)...")
    options(timeout = 3600); download.file(pbf_url, state, mode = "wb", quiet = TRUE)
  }
  status <- system2("osmium", c("extract", "--bbox", district_bbox, "--strategy", "complete_ways",
                                "-o", file.path(district, "district.osm.pbf"), state, "--overwrite"))
  if (status != 0) stop("osmium could not crop the road data.", call. = FALSE)
}
network <- r5r::build_network(district, verbose = FALSE)

# ---- stored Google data ------------------------------------------------------

con <- db_connect(); on.exit(DBI::dbDisconnect(con))
stations <- db_read_stations(con)
incidents <- db_read_incidents(con)
routes <- DBI::dbGetQuery(con, "SELECT event, station, distance_km, duration_free_flow_min FROM routes WHERE error IS NULL")
centre <- stations[stations$station == home, ]
google_grid <- cell_time_matrix(within_radius(db_read_cell_times(con), centre, radius_km))
cells <- unique(db_read_cell_times(con)[c("cell_id", "lat", "lng")])
cells <- cells[cells$cell_id %in% rownames(google_grid), ]
message(nrow(routes), " incident routes and ", nrow(cells), " grid cells within ", radius_km, " km to compare, ",
        "for ", nrow(stations), " stations")

# ---- r5r runs ----------------------------------------------------------------

pair_ids <- as.character(seq_len(nrow(routes)))
at_station <- stations[match(routes$station, stations$station), ]
at_incident <- incidents[match(routes$event, incidents$incident), ]
route_origins <- data.frame(id = pair_ids, lon = at_station$lng, lat = at_station$lat)
route_destinations <- data.frame(id = pair_ids, lon = at_incident$lng, lat = at_incident$lat)

run_r5 <- function(origins, destinations, all_to_all, new_carspeeds = NULL, carspeed_scale = 1) {
  it <- r5r::detailed_itineraries(network, origins, destinations, mode = "CAR", departure_datetime = departure,
                                  shortest_path = TRUE, all_to_all = all_to_all, new_carspeeds = new_carspeeds,
                                  carspeed_scale = carspeed_scale, drop_geometry = TRUE, verbose = FALSE, progress = FALSE)
  unique(as.data.frame(it)[c("from_id", "to_id", "total_duration", "total_distance")])
}

grid_origins <- data.frame(id = stations$station, lon = stations$lng, lat = stations$lat)
grid_destinations <- data.frame(id = cells$cell_id, lon = cells$lng, lat = cells$lat)

# Speeds per OSM way for the "class speeds" variant: the way's class and tags come from the
# road data itself; only ways r5r can drive on are listed.
ways <- sf::st_drop_geometry(sf::st_read(file.path(district, "district.osm.pbf"), quiet = TRUE,
  query = "SELECT osm_id, highway, other_tags FROM lines WHERE highway IS NOT NULL"))
car_edges <- sf::st_drop_geometry(r5r::street_network_to_sf(network)$edges)
car_ways <- unique(as.character(car_edges$osm_id[car_edges$car]))
ways <- ways[ways$osm_id %in% car_ways, ]
class_speed_table <- data.frame(osm_id = as.numeric(ways$osm_id), max_speed = way_speeds(ways$highway, ways$other_tags),
                                speed_type = "km/h", stringsAsFactors = FALSE)

evaluate <- function(label, new_carspeeds = NULL, carspeed_scale = 1) {
  message("Running variant: ", label)
  r <- run_r5(route_origins, route_destinations, FALSE, new_carspeeds, carspeed_scale)
  routed <- r$total_duration[match(pair_ids, r$from_id)]
  g <- run_r5(grid_origins, grid_destinations, TRUE, new_carspeeds, carspeed_scale)
  grid <- matrix(NA_real_, nrow(google_grid), ncol(google_grid), dimnames = dimnames(google_grid))   # NA: r5r found no route
  grid[cbind(match(g$to_id, rownames(grid)), match(g$from_id, colnames(grid)))] <- g$total_duration
  list(label = label, routed = routed, grid = grid, unreachable_cells = sum(rowSums(!is.na(grid)) == 0),
       missing_pairs = mean(is.na(grid)),
       routes_missing = sum(is.na(routed)),
       route_errors = time_errors(routed, routes$duration_free_flow_min),
       station_ratios = ratio_by(routed, routes$duration_free_flow_min, routes$station),
       grid_errors = time_errors(as.vector(grid), as.vector(google_grid)),
       agreement = rbind(`delay 0` = rank_agreement(google_grid, grid, home),
                         `delay 4` = rank_agreement(google_grid, grid, home, home_delay_min = 4)))
}

defaults <- evaluate("defaults")
scale <- defaults$route_errors$median_ratio                 # r5r is `scale` times slower, so speed it up by that much
variants <- list(defaults, evaluate("global scale", carspeed_scale = scale),
                 evaluate("class speeds", new_carspeeds = class_speed_table))

# ---- report ------------------------------------------------------------------

summary <- do.call(rbind, lapply(variants, function(v) data.frame(
  variant = v$label,
  routes = v$route_errors$n, routes_missing = v$routes_missing, route_median_ratio = v$route_errors$median_ratio, route_mae_min = v$route_errors$mae_min,
  route_p90_min = v$route_errors$p90_abs_min, route_correlation = v$route_errors$correlation,
  station_ratio_min = min(v$station_ratios), station_ratio_max = max(v$station_ratios),
  grid_pairs = v$grid_errors$n, grid_missing_share = v$missing_pairs, cells_unreachable = v$unreachable_cells, grid_median_ratio = v$grid_errors$median_ratio, grid_mae_min = v$grid_errors$mae_min,
  grid_p90_min = v$grid_errors$p90_abs_min,
  same_zone = v$agreement["delay 0", "same_zone"], within_one_zone = v$agreement["delay 0", "within_one_zone"],
  home_first_google = v$agreement["delay 0", "home_first_reference"], home_first_r5r = v$agreement["delay 0", "home_first_candidate"],
  home_first_both = v$agreement["delay 0", "home_first_both"],
  home_second_google = v$agreement["delay 0", "home_second_reference"], home_second_r5r = v$agreement["delay 0", "home_second_candidate"],
  home_second_both = v$agreement["delay 0", "home_second_both"],
  same_zone_delay4 = v$agreement["delay 4", "same_zone"], same_first_brigade = v$agreement["delay 0", "same_first_brigade"])))
dir.create("results", showWarnings = FALSE)
write.csv(summary, "results/r5r_comparison_summary.csv", row.names = FALSE)
by_station <- do.call(rbind, lapply(variants, function(v) data.frame(variant = v$label, station = names(v$station_ratios),
                                                                     median_ratio = as.vector(v$station_ratios))))
write.csv(by_station, "results/r5r_comparison_by_station.csv", row.names = FALSE)
write.csv(data.frame(routes, google_min = routes$duration_free_flow_min,
                     defaults_min = variants[[1]]$routed, global_scale_min = variants[[2]]$routed, class_speeds_min = variants[[3]]$routed),
          "results/r5r_comparison_routes.csv", row.names = FALSE)

options(width = 200); cat("\nOne column per variant (r5r time / Google free-flow time)\n")
print(t(round(summary[-1], 2)) |> `colnames<-`(summary$variant))
cat("\nMedian r5r/Google time ratio by station\n")
print(round(reshape(by_station, idvar = "station", timevar = "variant", direction = "wide")[, -1], 2) |>
        `rownames<-`(unique(by_station$station)))
