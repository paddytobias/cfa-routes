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

# the UI builds
stopifnot(inherits(app_ui(), "shiny.tag.list") || inherits(app_ui(), "shiny.tag"))
cat("app tests passed\n")
