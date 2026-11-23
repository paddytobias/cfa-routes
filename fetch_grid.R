# Work out travel times from every station to a grid of points around Chewton, so the
# app can map where each brigade would arrive first and second. Needs
# GOOGLE_MAPS_API_KEY (see run.R) with the Routes and Geocoding APIs enabled.
#
#   Rscript fetch_grid.R           # pilot: 600 station/cell pairs (about 100 cells)
#   Rscript fetch_grid.R 20000     # up to 20,000 pairs; re-running resumes where it stopped
#
# Google bills per pair (about 2,000 cells x 6 stations = 12,000 for the default grid), so
# the pilot size is the default. Cells nearest Chewton are fetched first; pairs already
# stored are never fetched again. To add a brigade, add it to config/stations.csv and re-run:
# only its column is fetched.

for (f in list.files("R", full.names = TRUE)) source(f)

args <- commandArgs(trailingOnly = TRUE)
max_elements <- if (length(args)) as.integer(args[[1]]) else 600L
home <- "Chewton"
radius_km <- 25
spacing_km <- 1

con <- db_connect()
client <- google_client(google_key())

db_upsert_stations(con, read.csv("config/stations.csv", stringsAsFactors = FALSE))
stations <- geocode_missing(con, client, "stations", "station", db_read_stations(con))
centre <- stations[stations$station == home, ]
print(stations[c("station", "lat", "lng")], row.names = FALSE)   # check the pins

db_add_cells(con, make_grid(centre$lat, centre$lng, radius_km, spacing_km))
message(nrow(db_read_cells(con)), " grid cells; ", nrow(db_missing_cell_pairs(con)), " station/cell pairs still to fetch")

n <- fetch_cell_times(con, client, max_elements)
message(n, " pair(s) fetched; ", nrow(db_missing_cell_pairs(con)), " still to fetch")

DBI::dbDisconnect(con)
