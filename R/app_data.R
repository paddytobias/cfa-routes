# Chewton's turnout area, if the GeoJSON file is there: the text for the map plus a caption
# built from the first feature's properties. A missing file means no overlay; a file that
# isn't valid GeoJSON warns and is skipped, so a bad file can't stop the app starting.
read_turnout_area <- function(path) {
  if (!file.exists(path)) return(NULL)
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  parsed <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(parsed) || !parsed$type %in% c("FeatureCollection", "Feature", "Polygon", "MultiPolygon")) {
    warning(sprintf("%s isn't valid GeoJSON; the turnout area won't be drawn.", path), call. = FALSE)
    return(NULL)
  }
  props <- if (identical(parsed$type, "FeatureCollection")) parsed$features[[1]]$properties else parsed$properties
  caption <- paste(c(props$description, if (!is.null(props$source)) sprintf("(%s)", props$source)), collapse = " ")
  list(geojson = text, caption = if (nzchar(caption)) caption else "Turnout area")
}

# Everything the app needs, read once at startup. The only database access the app makes.
load_case_data <- function(con, turnout_path = "data/turnout_area.geojson") {
  list(
    comparison = db_read_comparison(con),
    stations = db_read_stations(con),
    incidents = db_read_incidents(con),
    points = db_read_incidents(con)[c("incident", "lat", "lng")],
    polylines = db_read_polylines(con),
    cell_times = db_read_cell_times(con),
    turnout_area = read_turnout_area(turnout_path)
  )
}
