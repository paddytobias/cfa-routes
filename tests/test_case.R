# Run: Rscript tests/test_case.R   (no network, no database)
for (f in list.files("R", full.names = TRUE)) source(f)
tzm <- "Australia/Melbourne"

# One incident per scenario. Columns mirror db_read_comparison().
mk <- function(incident, etas, paged, n_paged = length(paged), t = "2026-11-21 16:06:05") {
  data.frame(incident = incident, event_time = as.POSIXct(t, tz = tzm), incident_address = paste("ADDR", incident),
             paged = paste(paged, collapse = ", "), n_paged = n_paged, station = names(etas),
             station_paged = names(etas) %in% paged, duration_traffic_min = unname(etas),
             distance_km = unname(etas) / 2, traffic_delay_min = 1, row.names = NULL, stringsAsFactors = FALSE)
}
v1 <- function(comp, ...) incident_verdicts(comp, ...)$verdict

# verdicts, one by one (Chewton is never paged here: its pager isn't on the feed)
stopifnot(v1(mk("a", c(Chewton = 5, Castlemaine = 8, Harcourt = 12), c("Castlemaine", "Harcourt"))) == "Chewton first")
stopifnot(v1(mk("b", c(Chewton = 9, Castlemaine = 8, Harcourt = 12), c("Castlemaine", "Harcourt"))) == "Within margin")   # 1 min behind
stopifnot(v1(mk("c", c(Chewton = 11, Castlemaine = 8, Harcourt = 12), c("Castlemaine", "Harcourt"))) == "Ahead of some paged")
stopifnot(v1(mk("d", c(Chewton = 15, Castlemaine = 8, Harcourt = 12), c("Castlemaine", "Harcourt"))) == "Slower than all paged")
stopifnot(v1(mk("e", c(Chewton = 5, Castlemaine = 8), "Chewton")) == "Chewton paged")
stopifnot(v1(mk("f", c(Chewton = 5, Castlemaine = 8), character(), n_paged = 0L)) == "No comparison")
stopifnot(v1(mk("g", c(Castlemaine = 8), "Castlemaine")) == "No comparison")                       # Chewton never routed
stopifnot(v1(mk("h", c(Chewton = NA, Castlemaine = 8), "Castlemaine")) == "No comparison")          # Chewton route failed

# parameters: margin, handicap (Chewton only), speed factor (everyone)
b <- mk("b", c(Chewton = 9, Castlemaine = 8, Harcourt = 12), c("Castlemaine", "Harcourt"))
stopifnot(v1(b, margin_min = 0.5) == "Ahead of some paged")
stopifnot(v1(mk("a", c(Chewton = 5, Castlemaine = 8), "Castlemaine"), handicap_min = 4) == "Within margin")   # 9 vs 8
stopifnot(v1(mk("a", c(Chewton = 5, Castlemaine = 8), "Castlemaine"), handicap_min = 4, margin_min = 0) == "Slower than all paged")
r <- incident_verdicts(b, speed_factor = 0.5); stopifnot(r$chewton_eta == 18, r$first_paged_eta == 16)

# fields
a <- incident_verdicts(mk("a", c(Chewton = 5, Castlemaine = 8, Harcourt = 12, Taradale = 3), c("Castlemaine", "Harcourt")))
stopifnot(a$minutes_ahead == 3, a$first_paged == "Castlemaine", a$chewton_rank == 1, a$beats_some,
          a$n_compared == 2, a$complete, a$paged_etas == "Castlemaine 8.0, Harcourt 12.0")    # unpaged Taradale ignored
inc <- incident_verdicts(mk("i", c(Chewton = 5, Castlemaine = 8), c("Castlemaine", "Malmsbury"), n_paged = 2L))
stopifnot(!inc$complete, inc$n_compared == 1)                      # a paged brigade we couldn't route
stopifnot(incident_verdicts(mk("d", c(Chewton = 15, Castlemaine = 8, Harcourt = 12), c("Castlemaine", "Harcourt")))$chewton_rank == 3)
stopifnot(is.null(incident_verdicts(mk("a", c(Chewton = 5), "x")[0, ])))

# several incidents: newest first, summary and headline
comp <- rbind(mk("a", c(Chewton = 5, Castlemaine = 8), "Castlemaine", t = "2026-11-10 10:00:00"),
              mk("c", c(Chewton = 11, Castlemaine = 8, Harcourt = 12), c("Castlemaine", "Harcourt"), t = "2026-11-12 10:00:00"),
              mk("d", c(Chewton = 15, Castlemaine = 8), "Castlemaine", t = "2026-11-14 10:00:00"),
              mk("e", c(Chewton = 5, Castlemaine = 8), "Chewton", t = "2026-11-16 10:00:00"))
vs <- incident_verdicts(comp)
stopifnot(identical(vs$incident, c("e", "d", "c", "a")))
s <- case_summary(vs)
stopifnot(s$incidents == 4, s$comparable == 3, s$chewton_paged == 1, s$first == 1, s$beats_some == 2, s$slower == 1,
          s$within_margin == 0, s$median_minutes_ahead == -3)    # (3, -3, -7) -> -3
h <- headline(s, 2)
stopifnot(grepl("1 of 3 \\(33%\\)", h), grepl("slower than every paged brigade in 1 of 3", h), grepl("-3.0 min", h))
stopifnot(grepl("No incidents", headline(case_summary(incident_verdicts(mk("e", c(Chewton = 5), "Chewton"))), 2)))

# detail table
d <- incident_detail(comp, "c", handicap_min = 1)
stopifnot(identical(d$Station, c("Castlemaine", "Chewton", "Harcourt")), d$`ETA (min)`[2] == 12, d$Paged[2] == "No")

# route paths: stored geometry vs straight-line fallback
stations <- data.frame(station = c("Chewton", "Castlemaine"), lat = c(-37.1, -37.0), lng = c(144.2, 144.3))
pt <- list(lat = -37.05, lng = 144.25)
poly <- googlePolylines::encode(data.frame(lat = c(-37.1, -37.07, -37.05), lon = c(144.2, 144.22, 144.25)))
pl <- data.frame(event = "c", station = "Chewton", polyline = poly)
paths <- route_paths("c", comp, stations, pt, pl)
stopifnot(length(paths) == 3, !paths[[1]]$straight, nrow(paths[[1]]$points) == 3, paths[[1]]$role == "chewton",
          paths[[2]]$straight, paths[[2]]$station == "Castlemaine", nrow(paths[[2]]$points) == 2)

# map builders return widgets without needing a browser
m <- add_routes(add_incident_markers(base_map(stations), vs, data.frame(incident = vs$incident, lat = -37.05, lng = 144.25)), paths)
stopifnot(inherits(m, "leaflet"), inherits(add_incident_markers(base_map(stations), NULL, NULL), "leaflet"))
stopifnot(all(levels(vs$verdict) %in% names(verdict_colours)))
cat("case tests passed\n")
