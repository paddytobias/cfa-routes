# Add road geometry to routes already stored, so the map can draw them. Costs one
# Routes API request per route (Chewton's and the paged brigades' only); times and
# distances already stored are not changed. Run again if more than max_routes remain.

for (f in list.files("R", full.names = TRUE)) source(f)

con <- db_connect()
on.exit(DBI::dbDisconnect(con))
n <- backfill_polylines(con, google_client(google_key()), max_routes = 50L)
message(n, " route(s) updated; ", nrow(db_pairs_without_polyline(con)), " still without geometry")

