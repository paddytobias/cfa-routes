# Turnout zones: for every grid cell, where would the home brigade arrive among
# the stations? Pure functions over the cell_times table; no I/O, no Shiny.

zone_levels <- c("First", "Second", "Third", "Fourth or later", "No data")

# How far from the home station the map shows. The stored grid reaches 25 km, but with only
# the listed brigades competing, squares far from every station say little about response areas.
zone_radius_km <- 12

# Rows of the long cell-times table whose cell lies within `radius_km` of `centre` (lat, lng).
within_radius <- function(cell_times, centre, radius_km = zone_radius_km) {
  km <- haversine_m(centre$lat, centre$lng, cell_times$lat, cell_times$lng) / 1000
  cell_times[km <= radius_km, , drop = FALSE]
}

# Long table (cell_id, station, duration_s) -> minutes, one row per cell and one
# column per station. A pair with no stored time is NA.
cell_time_matrix <- function(cell_times) {
  ok <- cell_times[!is.na(cell_times$duration_s), , drop = FALSE]
  if (nrow(ok) == 0) return(matrix(numeric(), 0, 0))
  tapply(ok$duration_s / 60, list(ok$cell_id, ok$station), mean)
}

# Rank the stations at each cell. Times are scaled by `speed_factor` (as in the
# incident analysis), `home_delay_min` is turnout time added to the home brigade
# only, and `excluded` stations are treated as unable to respond. `lead_min` is
# how far ahead of the best other brigade the home brigade is (negative: behind
# the first); a cell is `contested` when that lead is within `margin_min`.
rank_cells <- function(times_min, home = "Chewton", speed_factor = 1, home_delay_min = 0,
                       excluded = character(), margin_min = 2) {
  stopifnot(home %in% colnames(times_min))
  eta <- times_min / speed_factor
  eta[, home] <- eta[, home] + home_delay_min
  eta[, colnames(eta) %in% excluded] <- NA_real_
  one <- function(x) {
    others <- x[names(x) != home & !is.na(x)]
    arrivals <- sort(x[!is.na(x)])
    rank <- if (is.na(x[[home]])) NA_integer_ else 1L + sum(others < x[[home]])
    data.frame(
      home_eta = unname(x[[home]]), home_rank = rank,
      first = names(arrivals)[1], second = names(arrivals)[2],
      first_eta = unname(arrivals[1]), second_eta = unname(arrivals[2]),
      lead_min = if (is.na(rank) || length(others) == 0) NA_real_ else min(others) - x[[home]],
      stringsAsFactors = FALSE, row.names = NULL)
  }
  if (nrow(eta) == 0) return(NULL)
  out <- do.call(rbind, lapply(seq_len(nrow(eta)), function(i) one(eta[i, ])))
  zone <- ifelse(is.na(out$home_rank), "No data", zone_levels[pmin(out$home_rank, 4L)])
  cbind(cell_id = rownames(eta), out,
        zone = factor(zone, levels = zone_levels),
        contested = !is.na(out$lead_min) & abs(out$lead_min) <= margin_min,
        stringsAsFactors = FALSE)
}

# Share of mapped cells in each zone, for a headline figure.
zone_summary <- function(zones) {
  mapped <- zones[zones$zone != "No data", ]
  table(factor(mapped$zone, levels = setdiff(zone_levels, "No data")))
}
