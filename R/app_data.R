# Everything the app needs, read once at startup. The only database access the app makes.
load_case_data <- function(con) {
  list(
    comparison = db_read_comparison(con),
    stations = db_read_stations(con),
    points = db_read_incidents(con)[c("incident", "lat", "lng")],
    polylines = db_read_polylines(con)
  )
}
