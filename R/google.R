# Google Maps Platform access. The transport is the only impure piece;
# everything else builds requests or parses responses.

routes_url <- "https://routes.googleapis.com/directions/v2:computeRoutes"
route_field_mask <- paste(
  "routes.distanceMeters", "routes.duration", "routes.staticDuration",
  "routes.polyline.encodedPolyline", "routes.travelAdvisory.speedReadingIntervals",
  sep = ","
)

matrix_url <- "https://routes.googleapis.com/distanceMatrix/v2:computeRouteMatrix"
matrix_field_mask <- "originIndex,destinationIndex,duration,distanceMeters,status,condition"

curl_transport <- function(method, url, body = NULL, headers = list(), insecure = FALSE) {
  h <- curl::new_handle(customrequest = method)
  if (insecure) curl::handle_setopt(h, ssl_verifypeer = FALSE, ssl_verifyhost = 0L)
  if (!is.null(body)) {
    curl::handle_setopt(h, postfields = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE)))
    headers[["Content-Type"]] <- "application/json"
  }
  curl::handle_setheaders(h, .list = headers)
  res <- curl::curl_fetch_memory(url, h)
  parsed <- jsonlite::fromJSON(rawToChar(res$content), simplifyVector = FALSE)
  if (res$status_code >= 400) {
    msg <- parsed$error$message %||% paste("HTTP", res$status_code)
    stop(structure(
      class = c(if (res$status_code %in% c(429, 500, 502, 503, 504)) "transient_http_error",
                "http_error", "error", "condition"),
      list(message = msg, call = NULL)
    ))
  }
  parsed
}

# Function operator: retry a transport on transient HTTP errors only.
with_retry <- function(transport, times = 3L, delay = 1) {
  force(transport); force(times); force(delay)
  function(...) {
    for (i in seq_len(times)) {
      result <- tryCatch(transport(...), transient_http_error = function(e) e)
      if (!inherits(result, "transient_http_error")) return(result)
      if (i < times) Sys.sleep(delay * 2^(i - 1))
    }
    stop(result)
  }
}

# Function factory: bind the key and transport once, return the API verbs.
google_client <- function(key, transport = with_retry(curl_transport)) {
  force(key); force(transport)
  query_url <- function(base, ...) {
    args <- c(list(...), key = key)
    paste0(base, "?", paste(names(args), vapply(args, curl::curl_escape, ""), sep = "=", collapse = "&"))
  }
  list(
    routes = function(body) transport(
      "POST", routes_url, body,
      headers = list("X-Goog-Api-Key" = key, "X-Goog-FieldMask" = route_field_mask)
    ),
    matrix = function(body) transport(
      "POST", matrix_url, body,
      headers = list("X-Goog-Api-Key" = key, "X-Goog-FieldMask" = matrix_field_mask)
    ),
    elevation = function(encoded_path, samples) transport(
      "GET", query_url("https://maps.googleapis.com/maps/api/elevation/json",
                       path = paste0("enc:", encoded_path), samples = samples)
    ),
    geocode = function(address) transport(
      "GET", query_url("https://maps.googleapis.com/maps/api/geocode/json",
                       address = address, region = "au")
    )
  )
}

google_key <- function() {
  key <- Sys.getenv("GOOGLE_MAPS_API_KEY")
  if (!nzchar(key)) stop("Set GOOGLE_MAPS_API_KEY in ~/.Renviron (see run.R).", call. = FALSE)
  key
}

# ---- Pure request builders / parsers ---------------------------------------

waypoint <- function(p) list(location = list(latLng = list(latitude = p$lat, longitude = p$lng)))

routes_request <- function(origin, destination, departure_time) {
  list(
    origin = waypoint(origin),
    destination = waypoint(destination),
    travelMode = "DRIVE",
    routingPreference = "TRAFFIC_AWARE_OPTIMAL",
    departureTime = format(departure_time, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    extraComputations = list("TRAFFIC_ON_POLYLINE"),
    units = "METRIC"
  )
}

# One origin to many destinations, free-flow (no traffic): Google bills per element
# and allows more elements per request when traffic is not requested.
matrix_request <- function(origin, destinations) {
  endpoint <- function(p) list(waypoint = waypoint(p))
  list(
    origins = list(endpoint(origin)),
    destinations = lapply(seq_len(nrow(destinations)), function(i) endpoint(destinations[i, ])),
    travelMode = "DRIVE",
    routingPreference = "TRAFFIC_UNAWARE",
    units = "METRIC"
  )
}

# The response is an array of elements, one per destination. Proto3 JSON omits zero
# values, so a missing destinationIndex means 0. Destinations Google leaves out of the
# response come back as error rows, so they are retried rather than silently dropped.
parse_matrix <- function(resp, station, cell_ids) {
  one <- function(el) {
    ok <- identical(el$condition, "ROUTE_EXISTS")
    data.frame(
      cell_id = cell_ids[(el$destinationIndex %||% 0L) + 1L], station = station,
      duration_s = if (ok) parse_duration_s(el$duration) else NA_real_,
      distance_m = if (ok) el$distanceMeters %||% 0 else NA_real_,
      error = if (ok) NA_character_ else el$status$message %||% el$condition %||% "No route",
      stringsAsFactors = FALSE
    )
  }
  rows <- do.call(rbind, c(list(data.frame(cell_id = character(), station = character(), duration_s = numeric(),
                                           distance_m = numeric(), error = character())),
                           lapply(resp, one)))
  absent <- setdiff(cell_ids, rows$cell_id)
  if (length(absent)) {
    rows <- rbind(rows, data.frame(cell_id = absent, station = station, duration_s = NA_real_,
                                   distance_m = NA_real_, error = "Missing from response"))
  }
  rows
}

parse_route <- function(resp) {
  route <- resp$routes[[1]] %||% stop("No route found between these points.", call. = FALSE)
  polyline <- route$polyline$encodedPolyline
  shares <- speed_shares(decode_polyline(polyline), route$travelAdvisory$speedReadingIntervals)
  traffic_min <- parse_duration_s(route$duration) / 60
  static_min <- parse_duration_s(route$staticDuration) / 60
  data.frame(
    distance_km = route$distanceMeters / 1000,
    duration_traffic_min = traffic_min,
    duration_free_flow_min = static_min,
    traffic_delay_min = traffic_min - static_min,
    t(shares),
    polyline = polyline
  )
}

check_status <- function(resp, what) {
  if (!identical(resp$status, "OK")) {
    stop(sprintf("%s failed: %s %s", what, resp$status, resp$error_message %||% ""), call. = FALSE)
  }
  resp
}

parse_geocode <- function(resp) {
  loc <- check_status(resp, "Geocoding")$results[[1]]$geometry$location
  list(lat = loc$lat, lng = loc$lng)
}

# Elevation gain/loss and steepest climb along the sampled path.
parse_elevation <- function(resp) {
  res <- check_status(resp, "Elevation")$results
  pts <- data.frame(
    lat = vapply(res, function(r) r$location$lat, 0),
    lng = vapply(res, function(r) r$location$lng, 0),
    elev = vapply(res, function(r) r$elevation, 0)
  )
  rise <- diff(pts$elev)
  run <- segment_lengths_m(pts)
  data.frame(
    elev_gain_m = sum(pmax(rise, 0)),
    elev_loss_m = -sum(pmin(rise, 0)),
    elev_max_m = max(pts$elev),
    elev_min_m = min(pts$elev),
    max_climb_grade_pct = 100 * max(c(0, rise[run > 0] / run[run > 0]))
  )
}
