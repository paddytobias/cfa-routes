# Shiny UI and server. The server is a function of the data, so it can be tested
# without a database (see tests/test_app.R).

limits_text <- list(
  "Times are Google's predicted travel times for the same weekday and time of day next week, not the traffic on the night. They are for ordinary cars at legal speeds, not a fire appliance with lights and sirens (use the speed factor to adjust).",
  "Times run from each station to the incident. They exclude time to get crews to the station and the truck out. Use the turnout slider to see whether Chewton's lead survives a delay that others don't have.",
  "Only the six brigades in this analysis can be compared. If a paged brigade isn't one of them, the comparison is incomplete and is hidden by default.",
  "Chewton's pager isn't on the feed these incidents come from, so incidents where only Chewton was paged are not in the data. The incidents shown are ones where neighbouring brigades were paged, which are mostly in their areas. They are not a random sample of the district.",
  "The list shows every incident, including those where Chewton would have been slower."
)

app_ui <- function() {
  shiny::fluidPage(
    shiny::titlePanel("Chewton CFA: would we have been there first?"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::sliderInput("handicap", "Extra time for Chewton to turn out (min)", 0, 10, 0, step = 0.5),
        shiny::sliderInput("margin", "Within margin: behind the first paged brigade by up to (min)", 0, 5, 2, step = 0.5),
        shiny::sliderInput("speed", "Speed factor, all brigades (0.9 = trucks 10% slower than Google's car time)", 0.6, 1.2, 1, step = 0.05),
        shiny::checkboxInput("complete_only", "Only incidents where every paged brigade could be compared", TRUE),
        shiny::hr(),
        shiny::selectInput("incident", "Incident (or click a marker)", choices = NULL),
        shiny::downloadButton("download", "Download table (CSV)")
      ),
      shiny::mainPanel(
        width = 9,
        shiny::textOutput("headline"),
        shiny::br(),
        leaflet::leafletOutput("map", height = 520),
        shiny::h4("Selected incident"),
        shiny::tableOutput("detail"),
        shiny::h4("All incidents"),
        shiny::tableOutput("table"),
        shiny::tags$details(
          shiny::tags$summary("How to read this, and its limits"),
          shiny::tags$p("On the map, a thick red line is Chewton's route and thin blue lines are the brigades that were paged. Dashed lines are straight-line placeholders where road geometry hasn't been stored."),
          do.call(shiny::tags$ul, lapply(limits_text, shiny::tags$li))
        )
      )
    )
  )
}

table_for_display <- function(v) {
  v <- v[order(-v$minutes_ahead, na.last = TRUE), ]
  data.frame(
    When = format(v$event_time, "%a %d %b %H:%M"), Incident = v$incident_address, Paged = v$paged,
    `Chewton ETA (min)` = v$chewton_eta, `First paged` = ifelse(is.na(v$first_paged), NA, sprintf("%s (%.1f min)", v$first_paged, v$first_paged_eta)),
    `Minutes ahead` = v$minutes_ahead, Verdict = as.character(v$verdict), check.names = FALSE
  )
}

app_server <- function(data, home = "Chewton") {
  force(data)
  function(input, output, session) {
    has_routes <- nrow(data$comparison) > 0

    all_verdicts <- shiny::reactive({
      incident_verdicts(data$comparison, home, input$speed, input$handicap, input$margin)
    })
    verdicts <- shiny::reactive({
      v <- all_verdicts()
      if (!is.null(v) && input$complete_only) v <- v[v$complete, ]
      v
    })
    selected_ok <- function() shiny::validate(
      shiny::need(has_routes, "No routes stored yet. Run extract_incidents.R, then run.R."),
      shiny::need(!is.null(verdicts()) && nrow(verdicts()) > 0, "No incidents match the current filters.")
    )

    output$headline <- shiny::renderText({
      selected_ok()
      excluded <- nrow(all_verdicts()) - nrow(verdicts())
      paste0(headline(case_summary(verdicts()), input$margin),
             if (excluded > 0) sprintf(" (%d incident%s excluded: a paged brigade couldn't be compared.)", excluded, if (excluded == 1) "" else "s"))
    })

    output$table <- shiny::renderTable({ selected_ok(); table_for_display(verdicts()) },
                                       striped = TRUE, digits = 1, na = "–")
    output$detail <- shiny::renderTable({
      selected_ok(); shiny::req(input$incident)
      incident_detail(data$comparison, input$incident, home, input$speed, input$handicap)
    }, striped = TRUE, digits = 1)

    output$download <- shiny::downloadHandler(
      filename = function() sprintf("chewton_vs_paged_%s.csv", format(Sys.Date())),
      content = function(file) { selected_ok(); write.csv(table_for_display(verdicts()), file, row.names = FALSE, na = "") }
    )

    output$map <- leaflet::renderLeaflet(base_map(data$stations))
    shiny::observe({
      add_incident_markers(leaflet::leafletProxy("map"), verdicts(), data$points)
    })

    # keep the picker in step with the filtered list, and let a marker click choose
    shiny::observe({
      v <- verdicts(); keep <- shiny::isolate(input$incident)
      choices <- if (is.null(v)) character() else
        setNames(v$incident, sprintf("%s: %s", format(v$event_time, "%d %b %H:%M"), substr(v$incident_address, 1, 50)))
      shiny::updateSelectInput(session, "incident", choices = choices,
                               selected = if (!is.null(keep) && keep %in% choices) keep else unname(choices)[1])
    })
    shiny::observeEvent(input$map_marker_click, {
      id <- input$map_marker_click$id
      if (!is.null(id) && id %in% verdicts()$incident) shiny::updateSelectInput(session, "incident", selected = id)
    })

    shiny::observe({
      shiny::req(input$incident)
      point <- as.list(data$points[data$points$incident == input$incident, c("lat", "lng")])
      paths <- route_paths(input$incident, data$comparison, data$stations, point, data$polylines, home)
      add_routes(leaflet::leafletProxy("map"), paths)
    })
  }
}
