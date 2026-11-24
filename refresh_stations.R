# Recalculate the routes and grid travel times of stations whose coordinates you changed
# directly in the stations table of data/cfa_routes.sqlite. Their stored results were
# measured from the old position, so this clears them and fetches them again from Google.
#
#   Rscript refresh_stations.R Maldon Metcalfe            # clear and refetch these stations
#   Rscript refresh_stations.R "Sutton Grange"            # quote names with spaces
#   Rscript refresh_stations.R --no-fetch Maldon          # only clear; fetch later with run.R and fetch_grid.R
#
# Google bills per route and per grid cell (about 2,000 cells per station plus one route
# per incident), so name only the stations that moved. Changing coordinates in
# config/stations.csv instead needs none of this: the next run notices and refetches.
# Needs GOOGLE_MAPS_API_KEY (see run.R) unless --no-fetch is given.

for (f in list.files("R", full.names = TRUE)) source(f)

args <- commandArgs(trailingOnly = TRUE)
fetch <- !"--no-fetch" %in% args
stations <- setdiff(args, "--no-fetch")
if (length(stations) == 0) stop("Name at least one station, e.g. Rscript refresh_stations.R Maldon", call. = FALSE)

client <- if (fetch) google_client(google_key())   # fail before clearing anything if the key is missing
con <- db_connect()
db_upsert_stations(con, read.csv("config/stations.csv", stringsAsFactors = FALSE))

current <- db_read_stations(con)
print(current[current$station %in% stations, c("station", "lat", "lng")], row.names = FALSE)   # check the pins

done <- refresh_stations(con, client, stations, fetch)
if (fetch) {
  message(done[["incidents"]], " incident(s) routed; ", done[["grid_pairs"]], " grid pair(s) fetched")
} else {
  message("Cleared ", paste(stations, collapse = ", "), "; run run.R and fetch_grid.R to fetch again")
}
DBI::dbDisconnect(con)
