# Route stored incidents from each brigade, save the results to the database,
# and compare brigades from there.
#
# Setup: add  GOOGLE_MAPS_API_KEY=<your key>  to ~/.Renviron (usethis::edit_r_environ()),
# restart R, and enable Routes API, Elevation API and Geocoding API on the key's project.
# Never paste the key into a script. Run extract_incidents.R first to load incidents.

for (f in list.files("R", full.names = TRUE)) source(f)

# Each incident costs one route request per station (one per station in config/stations.csv).
max_incidents <- 5
# Set FALSE if the key isn't enabled for the Elevation API; elevation columns will be NA.
with_elevation <- FALSE
# Time is scaled by this factor: 0.9 = truck 10% slower than Google's passenger-car time.
speed_factor <- 1
home <- "Chewton"

con <- db_connect()

client <- google_client(google_key())

# 1. Sync stations from config. Coordinates are geocoded once and saved.
db_upsert_stations(con, read.csv("config/stations.csv", stringsAsFactors = FALSE))

# 2. Geocode and route only what hasn't been routed successfully yet (newest first).
routed <- route_pending(con, client, max_incidents, home, speed_factor, with_elevation)
message(length(routed), " incident(s) routed")
print(db_read_stations(con)[c("station", "code", "lat", "lng")], row.names = FALSE)  # check the pins

# 3. Analyse from the database.
comparison <- db_read_comparison(con, home = home, speed_factor = speed_factor)
dir.create("results", showWarnings = FALSE)
write.csv(comparison, "results/route_comparison.csv", row.names = FALSE)

print(summarise_brigades(comparison), digits = 3, row.names = FALSE)
print(comparison[c("incident", "incident_address", "paged", "n_paged", "station", "distance_km", "eta_min",
                   "traffic_delay_min", "minutes_vs_home", "rank", "error")], digits = 3)
on.exit(DBI::dbDisconnect(con))