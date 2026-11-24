# Run: Rscript tests/test_compare.R   (no network, no r5r)
for (f in list.files("R", full.names = TRUE)) source(f)
near <- function(a, b, tol = 1e-6) stopifnot(isTRUE(all.equal(a, b, tolerance = tol)))

# maxspeed tags from GDAL's other_tags string
tags <- c('"maxspeed"=>"60","oneway"=>"no"', '"surface"=>"asphalt"', NA, '"maxspeed"=>"signals"', '"ref"=>"C708","maxspeed"=>"100"', '"maxspeed"=>"80 km/h"')
near(parse_maxspeed(tags), c(60, NA, NA, NA, 100, 80))
# tagged value wins, then the class, then the fallback
near(way_speeds(c("tertiary", "tertiary", "unclassified", "bridleway"), c('"maxspeed"=>"50"', NA, '"surface"=>"gravel"', NA)), c(50, 70, 60, 40))
near(way_speeds("bridleway", NA, fallback = 15), 15)

# time errors
e <- time_errors(c(10, 20, NA, 30), c(5, 10, 8, 15))
near(c(e$n, e$median_ratio, e$bias_min, e$mae_min, e$worst_min), c(3, 2, 10, 10, 15))
near(e$correlation, 1)
stopifnot(time_errors(c(5, 9), c(5, 10))$bias_min == -0.5, time_errors(1, 0)$n == 0)       # a zero reference is skipped
rb <- ratio_by(c(10, 12, 30, 40), c(5, 6, 10, 10), c("a", "a", "b", "b"))
near(as.vector(rb), c(2, 3.5)); stopifnot(identical(names(rb), c("a", "b")))

# rank agreement: stations x cells in minutes
ref <- matrix(c(5, 10, 8,  9, 6, 12,  20, 4, 7,  6, 7, 5), nrow = 4, byrow = TRUE,
              dimnames = list(c("w", "x", "y", "z"), c("Chewton", "Castlemaine", "Harcourt")))
same <- rank_agreement(ref, ref)
stopifnot(same$cells == 4, same$same_zone == 1, same$within_one_zone == 1, same$same_first_brigade == 1,
          same$home_first_reference == 1, same$home_first_both == 1)                      # Chewton is first only at w
stopifnot(rank_agreement(ref, ref * 1.6)$same_zone == 1)                                  # a uniform slowdown changes no ranks
slow_home <- ref; slow_home[, "Chewton"] <- slow_home[, "Chewton"] * 2                    # Chewton slower only: ranks move
off <- rank_agreement(ref, slow_home)
stopifnot(off$same_zone < 1, off$home_first_candidate < off$home_first_reference, off$home_first_both <= off$home_first_candidate)
stopifnot(rank_agreement(ref, slow_home, home_delay_min = 100)$same_zone == 1)             # a huge delay puts Chewton last either way: same zone
# cells or stations missing from one side are dropped
part <- ref[c("w", "x"), c("Chewton", "Castlemaine")]
stopifnot(rank_agreement(ref, part)$cells == 2)
part[1, 1] <- NA; stopifnot(rank_agreement(ref, part)$cells == 1)
cat("compare tests passed\n")
