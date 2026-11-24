# Helpers for judging another router's travel times against Google's, and for setting road
# speeds in an OpenStreetMap network (see compare_r5r.R). Pure; no I/O and no r5r needed.

# Speeds assumed for roads with no `maxspeed` tag, by OSM highway class (km/h). Typical rural
# Victorian limits, chosen in advance; they are not fitted to Google's times.
class_speeds <- c(
  motorway = 100, motorway_link = 60, trunk = 90, trunk_link = 60, primary = 90, primary_link = 60,
  secondary = 80, secondary_link = 50, tertiary = 70, tertiary_link = 50, unclassified = 60,
  residential = 45, living_street = 20, service = 25, track = 25
)

# The `maxspeed` tag in km/h from GDAL's `other_tags` string ("maxspeed"=>"60",...), or NA when
# it is absent or not a plain number ("signals", "none").
parse_maxspeed <- function(other_tags) {
  hit <- grepl('"maxspeed"=>"[0-9]+', other_tags)
  out <- rep(NA_real_, length(other_tags))
  out[hit] <- as.numeric(sub('^.*"maxspeed"=>"([0-9]+).*$', "\\1", other_tags[hit]))
  out
}

# Car speed for each way: its maxspeed tag if it has one, else its class's assumed speed, else `fallback`.
way_speeds <- function(highway, other_tags, speeds = class_speeds, fallback = 40) {
  tagged <- parse_maxspeed(other_tags)
  by_class <- unname(speeds[highway])
  ifelse(!is.na(tagged), tagged, ifelse(!is.na(by_class), by_class, fallback))
}

# How far candidate times are from reference times (both in minutes). `median_ratio` above 1
# means the candidate is slower. Pairs missing from either are ignored.
time_errors <- function(candidate, reference) {
  ok <- !is.na(candidate) & !is.na(reference) & reference > 0
  if (!any(ok)) {
    return(data.frame(n = 0L, median_ratio = NA_real_, bias_min = NA_real_, mae_min = NA_real_,
                      p90_abs_min = NA_real_, worst_min = NA_real_, correlation = NA_real_))
  }
  d <- candidate[ok] - reference[ok]
  data.frame(
    n = sum(ok), median_ratio = median(candidate[ok] / reference[ok]), bias_min = mean(d),
    mae_min = mean(abs(d)), p90_abs_min = unname(quantile(abs(d), 0.9)), worst_min = max(abs(d)),
    correlation = cor(candidate[ok], reference[ok]), row.names = NULL
  )
}

# Median candidate/reference ratio per group (e.g. station). A wide spread means the
# candidate is off by different amounts in different places, which distorts rankings.
ratio_by <- function(candidate, reference, group) {
  ok <- !is.na(candidate) & !is.na(reference) & reference > 0
  tapply(candidate[ok] / reference[ok], group[ok], median)
}

# Do two time matrices (cells x stations, minutes) put the home brigade in the same turnout
# zone? Cells or stations missing from either are dropped. A uniform speed difference changes
# no ranks, so this measures the relative errors between stations, which are what matter.
rank_agreement <- function(reference, candidate, home = "Chewton", home_delay_min = 0, margin_min = 2) {
  cells <- intersect(rownames(reference), rownames(candidate))
  stations <- intersect(colnames(reference), colnames(candidate))
  r <- reference[cells, stations, drop = FALSE]; k <- candidate[cells, stations, drop = FALSE]
  keep <- complete.cases(r) & complete.cases(k)
  a <- rank_cells(r[keep, , drop = FALSE], home, 1, home_delay_min, character(), margin_min)
  b <- rank_cells(k[keep, , drop = FALSE], home, 1, home_delay_min, character(), margin_min)
  rank_a <- pmin(a$home_rank, 4L); rank_b <- pmin(b$home_rank, 4L)
  data.frame(
    cells = nrow(a),
    same_zone = mean(a$zone == b$zone),
    within_one_zone = mean(abs(rank_a - rank_b) <= 1),
    same_first_brigade = mean(a$first == b$first),
    home_first_reference = sum(a$zone == "First"), home_first_candidate = sum(b$zone == "First"),
    home_first_both = sum(a$zone == "First" & b$zone == "First"),
    home_second_reference = sum(a$zone == "Second"), home_second_candidate = sum(b$zone == "Second"),
    home_second_both = sum(a$zone == "Second" & b$zone == "Second"), row.names = NULL
  )
}
