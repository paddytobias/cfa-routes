# Run: Rscript tests/test_app.R   (no network; drives the server with shiny::testServer)
for (f in list.files("R", full.names = TRUE)) source(f)
tzm <- "Australia/Melbourne"

mk <- function(incident, etas, paged, t, n_paged = length(paged)) data.frame(
  incident = incident, event_time = as.POSIXct(t, tz = tzm), incident_address = paste("ADDR", incident),
  paged = paste(paged, collapse = ", "), n_paged = n_paged, station = names(etas), station_paged = names(etas) %in% paged,
  duration_traffic_min = unname(etas), distance_km = unname(etas) / 2, traffic_delay_min = 1, stringsAsFactors = FALSE)
comparison <- rbind(
  mk("a", c(Chewton = 5, Castlemaine = 8), "Castlemaine", "2026-11-10 10:00:00"),                       # Chewton first
  mk("d", c(Chewton = 15, Castlemaine = 8), "Castlemaine", "2026-11-14 10:00:00"),                      # slower
  mk("x", c(Chewton = 5, Castlemaine = 8), c("Castlemaine", "Malmsbury"), "2026-11-16 10:00:00"))       # incomplete
data <- list(comparison = comparison,
             stations = data.frame(station = c("Chewton", "Castlemaine"), lat = c(-37.1, -37.0), lng = c(144.2, 144.3)),
             points = data.frame(incident = c("a", "d", "x"), lat = -37.05, lng = 144.25),
             polylines = data.frame(event = character(), station = character(), polyline = character()))
defaults <- list(handicap = 0, margin = 2, speed = 1, complete_only = TRUE)

shiny::testServer(app_server(data), {
  do.call(session$setInputs, defaults)
  stopifnot(grepl("Across 2 incidents", output$headline), grepl("1 of 2 \\(50%\\)", output$headline),
            grepl("1 incident excluded", output$headline))
  stopifnot(nrow(verdicts()) == 2, identical(unname(verdicts()$incident), c("d", "a")))
  session$setInputs(incident = "a")
  pos <- function(html, x) regexpr(x, html, fixed = TRUE)[[1]]
  stopifnot(pos(output$detail, "Chewton") > 0, pos(output$detail, "Chewton") < pos(output$detail, "Castlemaine"))  # fastest first

  session$setInputs(complete_only = FALSE)                                                    # include incomplete
  stopifnot(nrow(verdicts()) == 3, grepl("Across 3 incidents", output$headline), !grepl("excluded", output$headline))

  session$setInputs(handicap = 4)                                                             # 4 min turnout delay for Chewton
  stopifnot(verdicts()$verdict[verdicts()$incident == "a"] == "Within margin")
  tab <- table_for_display(verdicts())
  stopifnot(nrow(tab) == 3, names(tab)[1:2] == c("When", "Incident"), tab$Incident[3] == "ADDR d")   # most negative last
  dt <- incidents_datatable(verdicts())$x
  stopifnot(setequal(dt$data$Incident, c("ADDR a", "ADDR d", "ADDR x")),
            identical(dt$options$order[[1]], list(0L, "desc")),
            # the hidden key is the event time, aligned with the rows shown
            identical(dt$data$`Sort key`, as.numeric(verdicts()$event_time[match(dt$data$Incident, paste("ADDR", verdicts()$incident))])))

  session$setInputs(handicap = 0, speed = 0.5)                                                # everyone twice as slow: gaps double
  stopifnot(all.equal(verdicts()$minutes_ahead[verdicts()$incident == "a"], 6))
})

# empty database: a message, not an error
shiny::testServer(app_server(list(comparison = comparison[0, ], stations = data$stations, points = data$points[0, ],
                                  polylines = data$polylines)), {
  do.call(session$setInputs, defaults)
  err <- tryCatch(output$headline, error = conditionMessage)
  stopifnot(grepl("No routes stored", err))
})

# filters that remove everything: a different message
shiny::testServer(app_server(list(comparison = comparison[comparison$incident == "x", ], stations = data$stations,
                                  points = data$points, polylines = data$polylines)), {
  do.call(session$setInputs, defaults)
  stopifnot(grepl("No incidents match", tryCatch(output$headline, error = conditionMessage)))
})

# turnout zones: two cells, three stations; with no grid stored the tab says so
shiny::testServer(app_server(data), {
  do.call(session$setInputs, c(defaults, list(excluded = NULL, outline_contested = TRUE)))
  stopifnot(grepl("No grid stored", tryCatch(output$zone_note, error = conditionMessage)))
})
cells <- data.frame(cell_id = rep(c("c1", "c2"), each = 3), lat = rep(c(-37.08, -37.09), each = 3), lng = 144.26,
                    station = rep(c("Chewton", "Castlemaine", "Harcourt"), 2),
                    duration_s = 60 * c(5, 10, 8,  9, 6, 12), distance_m = 1000)
far <- transform(cells[cells$cell_id == "c1", ], cell_id = "far", lat = -37.30)   # about 24 km out: not shown
shiny::testServer(app_server(c(data, list(cell_times = rbind(cells, far)))), {
  do.call(session$setInputs, c(defaults, list(excluded = NULL, outline_contested = TRUE)))
  stopifnot(identical(as.character(zones()$zone), c("First", "Second")), grepl("Of 2 squares mapped within 12 km of Chewton", output$zone_note$html))
  stopifnot(!zone_map_ready())                                                       # tab not opened yet: nothing to draw on
  session$setInputs(zone_map_bounds = list(north = -37, south = -37.2, east = 144.4, west = 144.1))
  stopifnot(zone_map_ready())                                                        # map exists: tiles are drawn
  session$setInputs(zone_map_bounds = list(north = -37, south = -37.1, east = 144.3, west = 144.2))  # panning
  stopifnot(zone_map_ready())
  session$setInputs(handicap = 4)                                                   # Chewton slower to turn out
  stopifnot(identical(as.character(zones()$zone), c("Second", "Third")))
  session$setInputs(handicap = 0, excluded = "Castlemaine")                         # Castlemaine can't respond
  stopifnot(identical(as.character(zones()$zone), c("First", "First")))
})
m <- add_zone_cells(leaflet::leaflet(), rank_cells(cell_time_matrix(cells), "Chewton"),
                    cell_bounds(unique(cells[c("cell_id", "lat", "lng")]), 1), TRUE)
stopifnot(identical(vapply(m$x$calls, `[[`, "", "method"), c("clearGroup", "addRectangles")))

# turnout area: loaded from a GeoJSON file at startup; a missing file means no overlay,
# an invalid one warns and is skipped
ta <- read_turnout_area("data/turnout_area.geojson")
stopifnot(!is.null(ta), grepl("Chewton brigade primary turnout area", ta$caption), grepl("CFAMap118", ta$caption))
stopifnot(is.null(read_turnout_area(tempfile(fileext = ".geojson"))))
bad <- tempfile(fileext = ".geojson"); writeLines("not json", bad)
stopifnot(inherits(tryCatch(read_turnout_area(bad), warning = identity), "warning"))
zm <- zone_map(data$stations, "Chewton", ta)
methods <- vapply(zm$x$calls, `[[`, "", "method")
stopifnot("addGeoJSON" %in% methods, which(methods == "addMapPane") < which(methods == "addGeoJSON"))
stopifnot(!"addGeoJSON" %in% vapply(zone_map(data$stations)$x$calls, `[[`, "", "method"))
shiny::testServer(app_server(c(data, list(cell_times = cells, turnout_area = ta))), {
  do.call(session$setInputs, c(defaults, list(excluded = NULL, outline_contested = TRUE)))
  stopifnot(grepl("Dashed outline: CFA Chewton brigade primary turnout area", output$zone_note$html))
})
stopifnot(!grepl("official_zones", as.character(app_ui())))                              # the upload is gone

# the UI builds
stopifnot(inherits(app_ui(), "shiny.tag.list") || inherits(app_ui(), "shiny.tag"))
cat("app tests passed\n")
