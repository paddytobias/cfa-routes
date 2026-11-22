# The case for Chewton: pure functions over the comparison table from
# db_read_comparison(). No I/O, no Shiny.

verdict_levels <- c("Chewton first", "Within margin", "Ahead of some paged",
                    "Slower than all paged", "Chewton paged", "No comparison")

# Classify one incident. `rows` are its station rows from the comparison table.
# Times are Google's predicted car times, scaled by `speed_factor`; `handicap_min`
# is extra time added to Chewton only (turnout delay); `margin_min` is how close
# behind the first paged brigade still counts as "within margin".
incident_verdict <- function(rows, home, speed_factor, handicap_min, margin_min) {
  eta <- rows$duration_traffic_min / speed_factor
  is_home <- rows$station == home
  chewton_paged <- any(is_home & rows$station_paged %in% TRUE)
  chewton <- if (any(is_home & !is.na(eta))) eta[is_home & !is.na(eta)][1] + handicap_min else NA_real_
  paged <- !is_home & rows$station_paged %in% TRUE & !is.na(eta)
  paged_eta <- sort(setNames(eta[paged], rows$station[paged]))

  has_comparison <- !is.na(chewton) && length(paged_eta) > 0
  first <- if (has_comparison) min(paged_eta) else NA_real_
  verdict <- if (chewton_paged) "Chewton paged"
    else if (!has_comparison) "No comparison"
    else if (chewton < first) "Chewton first"
    else if (chewton <= first + margin_min) "Within margin"
    else if (chewton < max(paged_eta)) "Ahead of some paged"
    else "Slower than all paged"

  data.frame(
    incident = rows$incident[1], event_time = rows$event_time[1],
    incident_address = rows$incident_address[1], paged = rows$paged[1], n_paged = rows$n_paged[1],
    n_compared = length(paged_eta) + chewton_paged,
    complete = (length(paged_eta) + chewton_paged) >= rows$n_paged[1],
    chewton_eta = chewton, chewton_km = rows$distance_km[is_home][1],
    first_paged = if (has_comparison) names(paged_eta)[1] else NA_character_,
    first_paged_eta = first,
    minutes_ahead = first - chewton,                     # positive: Chewton arrives first
    beats_some = has_comparison && chewton < max(paged_eta),
    chewton_rank = if (has_comparison) 1L + sum(paged_eta < chewton) else NA_integer_,
    paged_etas = paste(sprintf("%s %.1f", names(paged_eta), paged_eta), collapse = ", "),
    verdict = factor(verdict, levels = verdict_levels),
    stringsAsFactors = FALSE
  )
}

# One row per incident, newest first.
incident_verdicts <- function(comparison, home = "Chewton", speed_factor = 1,
                              handicap_min = 0, margin_min = 2) {
  if (nrow(comparison) == 0) return(NULL)
  per_incident <- lapply(split(comparison, comparison$incident), incident_verdict,
                         home, speed_factor, handicap_min, margin_min)
  verdicts <- do.call(rbind, unname(per_incident))
  verdicts[order(verdicts$event_time, decreasing = TRUE), ]
}

# Counts over the incidents where a Chewton-vs-paged comparison exists.
case_summary <- function(verdicts) {
  comparable <- verdicts[verdicts$verdict %in% c("Chewton first", "Within margin",
                                                  "Ahead of some paged", "Slower than all paged"), ]
  v <- comparable$verdict
  list(
    incidents = nrow(verdicts),
    comparable = nrow(comparable),
    chewton_paged = sum(verdicts$verdict == "Chewton paged"),
    first = sum(v == "Chewton first"),
    within_margin = sum(v == "Within margin"),
    beats_some = sum(comparable$beats_some),
    slower = sum(v == "Slower than all paged"),
    median_minutes_ahead = if (nrow(comparable)) median(comparable$minutes_ahead) else NA_real_
  )
}

# Plain-English summary of case_summary(), stating the limits as well as the wins.
headline <- function(summary, margin_min) {
  if (summary$comparable == 0) return("No incidents with a Chewton-versus-paged comparison yet.")
  pct <- function(n) sprintf("%d of %d (%.0f%%)", n, summary$comparable, 100 * n / summary$comparable)
  sprintf(paste0(
    "Across %d incidents where Chewton was not paged: Chewton would have arrived before the first ",
    "paged brigade in %s; within %g min of it in a further %s; and ahead of at least one paged ",
    "brigade in %s. It would have been slower than every paged brigade in %s. ",
    "Median difference to the first paged brigade: %+.1f min (positive = Chewton earlier)."),
    summary$comparable, pct(summary$first), margin_min, pct(summary$within_margin),
    pct(summary$beats_some), pct(summary$slower), summary$median_minutes_ahead)
}

# Station-by-station table for one incident, scaled the same way as the verdicts.
incident_detail <- function(comparison, incident, home = "Chewton", speed_factor = 1, handicap_min = 0) {
  rows <- comparison[comparison$incident == incident, ]
  eta <- rows$duration_traffic_min / speed_factor + ifelse(rows$station == home, handicap_min, 0)
  out <- data.frame(Station = rows$station,
                    Paged = ifelse(rows$station_paged %in% TRUE, "Yes", "No"),
                    `ETA (min)` = eta, `Distance (km)` = rows$distance_km,
                    `Traffic delay (min)` = rows$traffic_delay_min, check.names = FALSE)
  out[order(out$`ETA (min)`), ]
}

# Lines to draw for one incident: Chewton's route and each paged brigade's. Uses
# stored road geometry when present, else a straight line (flagged `straight`).
route_paths <- function(incident, comparison, stations, incident_point, polylines, home = "Chewton") {
  rows <- comparison[comparison$incident == incident & !is.na(comparison$duration_traffic_min) &
                       (comparison$station == home | comparison$station_paged %in% TRUE), ]
  one_path <- function(station, role) {
    encoded <- polylines$polyline[polylines$event == incident & polylines$station == station]
    from <- stations[stations$station == station, ]
    pts <- if (length(encoded)) decode_polyline(encoded[1]) else
      data.frame(lat = c(from$lat, incident_point$lat), lng = c(from$lng, incident_point$lng))
    list(station = station, role = role, straight = !length(encoded), points = pts)
  }
  Map(one_path, rows$station, ifelse(rows$station == home, "chewton", "paged"), USE.NAMES = FALSE)
}
