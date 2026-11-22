# Run with:  shiny::runApp()   (from this folder)
# Reads data/cfa_routes.sqlite (or the file named in the CFA_DB environment variable).

for (f in list.files("R", full.names = TRUE)) source(f)

data <- local({
  con <- db_connect(Sys.getenv("CFA_DB", "data/cfa_routes.sqlite"))
  on.exit(DBI::dbDisconnect(con))
  load_case_data(con)
})

shiny::shinyApp(app_ui(), app_server(data))
