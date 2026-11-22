# Pure geometry and parsing helpers. No I/O.

haversine_m <- function(lat1, lng1, lat2, lng2) {
  rad <- pi / 180
  a <- sin((lat2 - lat1) * rad / 2)^2 +
    cos(lat1 * rad) * cos(lat2 * rad) * sin((lng2 - lng1) * rad / 2)^2
  2 * 6371000 * asin(pmin(1, sqrt(a)))
}

segment_lengths_m <- function(pts) {
  n <- nrow(pts)
  haversine_m(pts$lat[-n], pts$lng[-n], pts$lat[-1], pts$lng[-1])
}

decode_polyline <- function(encoded) {
  pts <- googlePolylines::decode(encoded)[[1]]
  data.frame(lat = pts$lat, lng = pts$lon)
}

parse_duration_s <- function(x) as.numeric(sub("s$", "", x))

# Share of route length (percent) at each Google traffic speed class.
# `intervals` is travelAdvisory.speedReadingIntervals; point indices are
# 0-based and an omitted start index means 0.
speed_shares <- function(pts, intervals) {
  seg <- segment_lengths_m(pts)
  label_segment <- function(labels, iv) {
    from <- iv$startPolylinePointIndex %||% 0L
    to <- min(iv$endPolylinePointIndex, length(seg))
    if (to > from) labels[seq.int(from + 1L, to)] <- iv$speed
    labels
  }
  labels <- Reduce(label_segment, intervals, rep("NORMAL", length(seg)))
  share <- function(cls) 100 * sum(seg[labels == cls]) / sum(seg)
  c(pct_normal = share("NORMAL"), pct_slow = share("SLOW"), pct_jam = share("TRAFFIC_JAM"))
}

# Next occurrence of weekday `wday` (1 = Monday ... 7 = Sunday) at `hour`:00
# in `tz`, strictly after `from`. Routes API needs a future departure time.
next_departure <- function(wday, hour, tz = "Australia/Melbourne", from = Sys.time()) {
  local <- as.POSIXlt(from, tz = tz)
  days_ahead <- (wday - (local$wday + 6L) %% 7L - 1L) %% 7L
  day <- as.Date(format(local, "%Y-%m-%d")) + days_ahead
  candidate <- as.POSIXct(sprintf("%s %02d:00:00", day, hour), tz = tz)
  if (candidate <= from) candidate + 7 * 86400 else candidate
}

# The same weekday and local clock time as `event_time`, in the first future
# week after `from` (plus `lead` seconds, so the request is never already past).
# Steps by calendar week, so the clock time survives daylight-saving changes.
match_event_time <- function(event_time, from = Sys.time(), lead = 300) {
  one <- function(t) {
    lt <- as.POSIXlt(t)
    weeks <- max(1, ceiling(as.numeric(difftime(from, t, units = "days")) / 7))
    repeat {
      shifted <- lt
      shifted$mday <- lt$mday + 7L * as.integer(weeks)
      candidate <- as.POSIXct(shifted)
      if (candidate > from + lead) return(candidate)
      weeks <- weeks + 1
    }
  }
  tz <- attr(as.POSIXlt(event_time[1]), "tzone")[1]
  .POSIXct(vapply(event_time, function(t) as.numeric(one(t)), 0), tz = tz)
}
