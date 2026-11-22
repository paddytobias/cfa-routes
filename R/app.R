# Shiny UI and server. The server is a function of the data, so it can be tested
# without a database (see tests/test_app.R).

limits_text <- list(
  "Times are Google's predicted travel times for the same weekday and time of day next week, not the traffic on the night. They are for ordinary cars at legal speeds, not a fire appliance with lights and sirens (use the speed factor to adjust).",
  "Times run from each station to the incident. They exclude time to get crews to the station and the truck out. Use the turnout slider to see whether Chewton's lead survives a delay that others don't have.",
  "Only the six brigades in this analysis can be compared. If a paged brigade isn't one of them, the comparison is incomplete and is hidden by default.",
  "Chewton's pager isn't on the feed these incidents come from, so incidents where only Chewton was paged are not in the data. The incidents shown are ones where neighbouring brigades were paged, which are mostly in their areas. They are not a random sample of the district.",
  "The list shows every incident, including those where Chewton would have been slower."
)

app_theme <- function() {
  bslib::bs_theme(
    version = 5, preset = "shiny", primary = "#B30000",
    base_font = bslib::font_google("Inter", local = FALSE),
    heading_font = bslib::font_google("Inter", wght = c(600, 700), local = FALSE)
  ) |>
    bslib::bs_add_rules("
      .bslib-value-box .value-box-title { font-size: .8rem; text-transform: uppercase; letter-spacing: .05em; opacity: .9; }
      .bslib-value-box .value-box-value { font-weight: 700; }
      .kpi-note { font-size: .85rem; color: var(--bs-secondary-color); margin: 0 0 .25rem; }
      .card { border: 0; box-shadow: 0 1px 3px rgba(0,0,0,.08), 0 1px 2px rgba(0,0,0,.06); }
      .card-header { font-weight: 600; background: transparent; }
      #map { min-height: 320px; max-height: 520px; }
      .table-responsive td, .table-responsive th { white-space: nowrap; }
      @media (max-width: 575.98px) {
        .navbar-brand { font-size: 1rem; white-space: normal; }
        .bslib-page-dashboard { padding: .5rem; }
        .bslib-value-box .value-box-showcase { display: none; }
        .bslib-value-box .value-box-value { font-size: 1.5rem; }
        .bslib-value-box .value-box-title { font-size: .7rem; }
        .bslib-value-box p { font-size: .75rem; margin-bottom: 0; }
        .card-body { padding: .75rem; }
      }
    ")
}

# Dashboard widgets for case_summary(): one value box per headline figure.
kpi_boxes <- function(s, margin) {
  pct <- function(n) if (s$comparable) sprintf("%.0f%%", 100 * n / s$comparable) else "–"
  box <- function(title, value, sub, bg, icon, fg = "#FFFFFF") bslib::value_box(
    title = title, value = value, shiny::p(sub), showcase = shiny::icon(icon),
    theme = bslib::value_box_theme(bg = bg, fg = fg), height = "auto")
  bslib::layout_column_wrap(
    width = "150px", fill = FALSE,
    box("Incidents compared", s$comparable, sprintf("%d where Chewton was not paged", s$comparable), "#343A40", "list-check"),
    box("Chewton first", pct(s$first), sprintf("%d of %d incidents", s$first, s$comparable), verdict_colours[["Chewton first"]], "trophy"),
    box("Within margin", pct(s$within_margin), sprintf("within %g min of the first brigade", margin), verdict_colours[["Within margin"]], "stopwatch", "#000000"),
    box("Ahead of some", pct(s$beats_some), "beats at least one paged brigade", verdict_colours[["Ahead of some paged"]], "ranking-star", "#000000"),
    box("Median difference", if (is.na(s$median_minutes_ahead)) "–" else sprintf("%+.1f min", s$median_minutes_ahead),
        "to first paged (+ = Chewton earlier)", "#6C757D", "clock")
  )
}

app_ui <- function() {
  bslib::page_navbar(
    title = "Chewton CFA: would we have been quicker?",
    theme = app_theme(),
    fillable = FALSE,
    sidebar = bslib::sidebar(
      width = 320,
      shiny::selectInput("incident", "Incident (or click a marker)", choices = NULL),
      shiny::hr(),
      shiny::sliderInput("handicap", "Extra time for Chewton to turn out (min)", 0, 10, 0, step = 0.5),
      shiny::sliderInput("margin", "Within X minutes of the first paged brigade's arrival", 0, 5, 2, step = 0.5),
      shiny::sliderInput("speed", "Speed factor, all brigades (0.9 = trucks 10% slower than Google's car time)", 0.6, 1.2, 0.9, step = 0.05),
      shiny::checkboxInput("complete_only", "Only incidents where every paged brigade could be compared", FALSE),
    
      shiny::downloadButton("download", "Download table (CSV)", class = "btn-outline-primary")
    ),
    bslib::nav_panel(
      "Overview", icon = shiny::icon("map-location-dot"), class = "bslib-page-dashboard",
      shiny::uiOutput("kpis"),
      bslib::card(
        bslib::card_header("Map"),
        full_screen = TRUE,
        leaflet::leafletOutput("map", height = "65vh")
      ),
      bslib::card(bslib::card_header("Selected incident"),
                  shiny::div(class = "table-responsive", shiny::tableOutput("detail")))
    ),
    bslib::nav_panel(
      "All incidents", icon = shiny::icon("table-list"), class = "bslib-page-dashboard",
      bslib::card(bslib::card_header("All incidents"), full_screen = TRUE,
                  shiny::div(class = "table-responsive", shiny::tableOutput("table")))
    ),
    bslib::nav_panel(
      "About", icon = shiny::icon("circle-info"), class = "bslib-page-dashboard",
      bslib::card(
        bslib::card_header("How to read this, and its limits"),
        shiny::tags$p("On the map, a thick red line is Chewton's route and thin blue lines are the brigades that were paged. Dashed lines are straight-line placeholders where road geometry hasn't been stored."),
        do.call(shiny::tags$ul, lapply(limits_text, shiny::tags$li))
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

    output$kpis <- shiny::renderUI({
      selected_ok()
      kpi_boxes(case_summary(verdicts()), input$margin)
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
