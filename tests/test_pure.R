# Run: Rscript tests/test_pure.R   (no network, no API key needed)
for (f in list.files("R", full.names = TRUE)) source(f)
near <- function(a, b, tol = 1e-6) stopifnot(isTRUE(all.equal(a, b, tolerance = tol)))

# geometry
near(haversine_m(0, 0, 0, 1), 111195, 1e-3)
poly <- googlePolylines::encode(data.frame(lat = c(0, 0, 0), lon = c(0, 0.01, 0.02)))
pts <- decode_polyline(poly)
iv <- list(list(endPolylinePointIndex = 1L, speed = "TRAFFIC_JAM"),
           list(startPolylinePointIndex = 1L, endPolylinePointIndex = 2L, speed = "SLOW"))
s <- speed_shares(pts, iv); near(unname(s), c(0, 50, 50), 1e-3)
near(unname(speed_shares(pts, NULL)), c(100, 0, 0))
near(parse_duration_s("1234s"), 1234)

# next_departure: strictly future, right weekday and hour
from <- as.POSIXct("2026-11-22 12:00:00", tz = "Australia/Melbourne")  # a Sunday
d <- next_departure(2, 8, from = from)
stopifnot(d > from, format(d, "%A %H", tz = "Australia/Melbourne") == "Tuesday 08")
stopifnot(next_departure(7, 3, from = from) - from > 0)

# parsing with fakes
resp <- list(routes = list(list(
  distanceMeters = 2000, duration = "300s", staticDuration = "240s",
  polyline = list(encodedPolyline = poly),
  travelAdvisory = list(speedReadingIntervals = iv))))
r <- parse_route(resp)
near(c(r$distance_km, r$duration_traffic_min, r$traffic_delay_min, r$pct_slow), c(2, 5, 1, 50), 1e-3)
elev <- list(status = "OK", results = lapply(c(100, 110, 105), function(e)
  list(elevation = e, location = list(lat = 0, lng = e / 1000))))
e <- parse_elevation(elev); near(c(e$elev_gain_m, e$elev_loss_m), c(10, 5))
stopifnot(inherits(tryCatch(parse_elevation(list(status = "REQUEST_DENIED")), error = identity), "error"))

# client + comparison end-to-end with a fake transport
fake <- function(method, url, body = NULL, headers = list()) {
  if (grepl("elevation", url)) return(elev)
  if (grepl("geocode", url)) return(list(status = "OK", results = list(list(geometry = list(location = list(lat = -37, lng = 144))))))
  stopifnot(body$routingPreference == "TRAFFIC_AWARE_OPTIMAL", headers[["X-Goog-Api-Key"]] == "k")
  resp
}
client <- google_client("k", fake)
st <- data.frame(station = c("Chewton", "Other"), address = c("a", "b"), lat = c(0, 0), lng = c(0, 0.01))
inc <- data.frame(incident = "x", address = "x", lat = 0, lng = 0.02)
dep <- data.frame(scenario = "s", departure_time = .POSIXct(Sys.time() + 3600))
out <- compare_brigades(client, st, inc, dep, samples = 3L)
stopifnot(nrow(out) == 2, all(out$rank == 1), all(out$minutes_vs_home == 0))
near(resolve_locations(client, data.frame(address = "a", lat = NA_real_, lng = NA_real_))$lat, -37)

# a failing route yields an error row, not an abort
bad <- google_client("k", function(...) stop("boom"))
out <- compare_brigades(bad, st, inc, dep)
stopifnot(all(out$error == "boom"), all(is.na(out$eta_min)))

# retry: transient errors retried, others not
calls <- 0
flaky <- function(...) { calls <<- calls + 1; if (calls < 3) stop(structure(class = c("transient_http_error", "error", "condition"), list(message = "x", call = NULL))); "ok" }
stopifnot(with_retry(flaky, times = 3, delay = 0)() == "ok", calls == 3)
calls <- 0
fatal <- function(...) { calls <<- calls + 1; stop("nope") }
try(with_retry(fatal, times = 3, delay = 0)(), silent = TRUE); stopifnot(calls == 1)
cat("all tests passed\n")

# ---- pagermon ----
msg <- function(text, ts = 1795313194) list(message = text, timestamp = ts)
a <- parse_alert(msg("@@ALERT CMAI6 G&SC1 FIRE IN SHRUBS CNR BULKELEYS RD/MALDON RD MCKENZIE HILL SVC 8187 J6 (499937) F CCAMP CCMAI CHARC F261124658 [CMAI]"))
stopifnot(a$event == "F261124658", a$primary_brigade == "CMAI", a$turnout == "CCAMP CCMAI CHARC",
          a$location_text == "FIRE IN SHRUBS CNR BULKELEYS RD/MALDON RD MCKENZIE HILL")
stopifnot(is.null(parse_alert(msg("QDHarcourt pager test number 11. [HARC]"))))
# SES job with a caller phone number is dropped, never parsed
stopifnot(is.null(parse_alert(msg("HbS261151747 CASE - FLOODING - 282 PYRENEES HWY CHEWTON - MAP: SVC 8137 J11 (572924) S CALLER: A B - 0413234837 [CASE]"))))
# same event paged to three brigades -> one row
dups <- list(msg(a$event), msg("@@ALERT CMAI1 STRUC1 RAIL SHED FIRE KENNEDY ST CASTLEMAINE SVC 8135 E6 (522945) F CCAMP CCMAI F261121940 [CAMP]"))
dups[[1]]$message <- "@@ALERT CMAI1 STRUC1 RAIL SHED FIRE KENNEDY ST CASTLEMAINE SVC 8135 E6 (522945) F CCAMP CCMAI F261121940 [CMAI]"
stopifnot(nrow(parse_alerts(dups)) == 1)
al <- parse_alerts(list(msg("@@ALERT CMAI1 STRUC1 RAIL SHED FIRE KENNEDY ST CASTLEMAINE SVC 8135 E6 (522945) F CCAMP CCMAI F261121940"),
                        msg("@@ALERT LOCK1 STRUC1 FURNACE 25 HILDA DR RAVENSWOOD SVC 8135 E6 (522945) F CLOCK F261107597")))
stopifnot(identical(is_local(al, "CCMAI"), c(TRUE, FALSE)) || identical(is_local(al[order(al$event), ], "CCMAI"), c(TRUE, FALSE)))
# address extraction
stopifnot(incident_address("INDOOR FURNACE FIRE 25 HILDA DR RAVENSWOOD /BEILHARZ RD") == "25 HILDA DR RAVENSWOOD, VIC, Australia",
          incident_address("FIRE IN SHRUBS CNR BULKELEYS RD/MALDON RD MCKENZIE HILL") == "CNR BULKELEYS RD/MALDON RD MCKENZIE HILL, VIC, Australia")
# paging: stops at pageCount, 1-based
pages <- list(list(init = list(pageCount = 2), messages = list(1, 2)), list(init = list(pageCount = 2), messages = list(3)))
seen <- integer(); pm <- pagermon_client("http://x", function(method, url, ...) { p <- as.integer(sub(".*page=", "", url)); seen <<- c(seen, p); pages[[p]] })
stopifnot(length(fetch_all_pages(pm, "q", pause = 0)) == 3, identical(seen, 1:2))
cat("pagermon tests passed\n")

stopifnot(is.null(read_archive(tempfile(fileext = ".csv"))))   # CSV import helper tolerates a missing file

# ---- column handling (regression: archive columns were read as lat/lng) ----
archive_like <- data.frame(event = "F1", time = Sys.time(), primary_brigade = "CAMP", response_code = "NOSTC3",
                           location_text = "x", map_ref = "m", turnout = "CCAMP", address_guess = "x")
err <- tryCatch(compare_brigades(client, st, archive_like, dep), error = conditionMessage)
stopifnot(grepl("Incidents needs columns", err), grepl("incidents_config", err))
err <- tryCatch(resolve_locations(client, archive_like), error = conditionMessage)
stopifnot(grepl("Locations needs columns", err))
# column order no longer matters
inc_shuffled <- inc[c("lng", "lat", "address", "incident")]
stopifnot(nrow(compare_brigades(client, st, inc_shuffled, dep, samples = 3L)) == 2)
# archive -> config -> geocoded numeric coordinates
cfg <- incidents_config(transform(archive_like, time = as.POSIXct("2026-11-21 16:06:05", tz = "Australia/Melbourne")))
stopifnot(identical(names(cfg), c("incident", "address", "lat", "lng", "event_time", "incident_address", "paged")))
stopifnot(is.numeric(resolve_locations(client, cfg)$lat))
# an address Google can't place -> NA + warning, not an abort
nogeo <- google_client("k", function(...) list(status = "ZERO_RESULTS", results = list()))
w <- NULL; r <- withCallingHandlers(resolve_locations(nogeo, cfg), warning = function(c) { w <<- conditionMessage(c); invokeRestart("muffleWarning") })
stopifnot(is.na(r$lat), grepl("Could not geocode", w))
cat("column-handling tests passed\n")

# ---- optional elevation ----
elev_calls <- 0
counting <- google_client("k", function(method, url, ...) {
  if (grepl("elevation", url)) { elev_calls <<- elev_calls + 1; stop("REQUEST_DENIED") }
  resp
})
out <- compare_brigades(counting, st, inc, dep, with_elevation = FALSE)
stopifnot(elev_calls == 0, all(is.na(out$error)), all(is.na(out$elev_gain_m)),
          !anyNA(out$duration_traffic_min), !anyNA(out$distance_km))
stopifnot(identical(names(out), names(compare_brigades(client, st, inc, dep, samples = 3L))))  # same shape
out <- compare_brigades(counting, st, inc, dep)   # elevation on + denied -> visible error rows
stopifnot(all(out$error == "REQUEST_DENIED"))
cat("optional-elevation tests passed\n")

# ---- event-time departures ----
tzm <- "Australia/Melbourne"
from <- as.POSIXct("2026-11-22 12:00:00", tz = tzm)                        # Sunday
ev <- as.POSIXct(c("2026-11-21 16:06:05", "2026-11-14 20:18:53", "2026-11-22 12:01:00"), tz = tzm)
m <- match_event_time(ev, from)
stopifnot(all(m > from), all(m - from <= 7 * 86400 + 300 + 1),
          format(m, "%a %H:%M:%S", tz = tzm) == format(ev, "%a %H:%M:%S", tz = tzm),
          attr(as.POSIXlt(m), "tzone")[1] == tzm)
# DST: an event before the 2027-04-04 change keeps its local clock time after it
dst <- match_event_time(as.POSIXct("2027-03-27 08:00:00", tz = tzm), as.POSIXct("2027-03-30 09:00:00", tz = tzm))
stopifnot(format(dst, "%Y-%m-%d %H:%M", tz = tzm) == "2027-04-03 08:00")
# end to end: scenario cross-join is gone by default; every row carries its own time
inc_t <- transform(inc, event_time = as.POSIXct("2026-11-21 16:06:05", tz = tzm))
out <- compare_brigades(client, st, inc_t, samples = 3L)
stopifnot(nrow(out) == 2, all(out$scenario == "event Sat 16:06"), all(out$departure_time > Sys.time()))
stopifnot(grepl("event_time", tryCatch(compare_brigades(client, st, inc, samples = 3L), error = conditionMessage)))
stopifnot(nrow(compare_brigades(client, st, inc, dep, samples = 3L)) == 2)      # fixed scenarios still work
cat("event-time tests passed\n")

# ---- reference columns ----
al <- parse_alerts(list(msg("@@ALERT CMAI1 STRUC1 RAIL SHED FIRE KENNEDY ST CASTLEMAINE /CAMPBELL ST SVC 8135 E6 (522945) F CCAMP CCMAI CHARC F261121940")))
cfg <- incidents_config(al)
stopifnot(cfg$paged == "CCAMP, CCMAI, CHARC", cfg$incident_address == "RAIL SHED FIRE KENNEDY ST CASTLEMAINE /CAMPBELL ST")
cfg$lat <- 0; cfg$lng <- 0.02
out <- compare_brigades(client, st, cfg, samples = 3L)
stopifnot(nrow(out) == 2, all(out$paged == "CCAMP, CCMAI, CHARC"), all(out$incident_address == cfg$incident_address),
          identical(names(out)[1:3], c("incident", "incident_address", "paged")))
stopifnot(!any(c("incident_address", "paged") %in% names(compare_brigades(client, st, inc, dep, samples = 3L))))  # absent -> omitted
cat("reference-column tests passed\n")
