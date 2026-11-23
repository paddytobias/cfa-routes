# Deploy the Shiny app to shinyapps.io.
#
# One-time setup:
#   1. install.packages("rsconnect")
#   2. In shinyapps.io: Account > Tokens > Show, then register it once in your R session:
#        rsconnect::setAccountInfo(name = "<account>", token = "<token>", secret = "<secret>")
#      or put SHINYAPPS_NAME, SHINYAPPS_TOKEN and SHINYAPPS_SECRET in ~/.Renviron and this
#      script registers them for you. Never paste the token or secret into a script.
#
# Run from this folder:  source("deploy.R")
# Re-run after extract_incidents.R / run.R / backfill_polylines.R to publish new data.
#
# Only the app, its code and the SQLite database are uploaded. The Google key is not needed
# by the app (routes and geometry are read from the database) and is never deployed.

app_name <- "cfa-routes"

# shinyapps.io has no environment-variable settings, so the app's variables travel in a
# .Renviron bundled with it. It is generated here with only the variables listed below
# (never copied from ~/.Renviron, which holds the Google key and shinyapps.io token) and
# removed once the deploy finishes.
app_env_vars <- "CARTO_BASEMAP_URL"

app_files <- c("app.R", ".Renviron", list.files("R", full.names = TRUE), "data/cfa_routes.sqlite",
               "data/turnout_area.geojson")

stopifnot(
  "Run this from the project folder." = file.exists("app.R"),
  "data/cfa_routes.sqlite not found; run run.R first." = file.exists("data/cfa_routes.sqlite"),
  "data/turnout_area.geojson not found; the Turnout zones map needs it." = file.exists("data/turnout_area.geojson")
)
if (!requireNamespace("rsconnect", quietly = TRUE)) stop("Install rsconnect first: install.packages('rsconnect')", call. = FALSE)

# Register credentials from the environment when they are set.
creds <- Sys.getenv(c("SHINYAPPS_NAME", "SHINYAPPS_TOKEN", "SHINYAPPS_SECRET"))
if (all(nzchar(creds))) {
  rsconnect::setAccountInfo(name = creds[["SHINYAPPS_NAME"]], token = creds[["SHINYAPPS_TOKEN"]],
                            secret = creds[["SHINYAPPS_SECRET"]])
}
if (nrow(rsconnect::accounts()) == 0) {
  stop("No shinyapps.io account registered; see the setup notes at the top of deploy.R.", call. = FALSE)
}

# Smoke-test that the data loads before uploading anything.
local({
  for (f in list.files("R", full.names = TRUE)) source(f)
  con <- db_connect()
  on.exit(DBI::dbDisconnect(con))
  d <- load_case_data(con)
  message("Data loads: ", nrow(d$comparison), " route rows, ", nrow(d$polylines), " with road geometry")
  stopifnot("data/turnout_area.geojson didn't load as GeoJSON." = !is.null(d$turnout_area))
})

env_values <- Sys.getenv(app_env_vars)
# Sys.getenv() only names its result when given several variables, so name it explicitly.
names(env_values) <- app_env_vars
missing_vars <- names(env_values)[!nzchar(env_values)]
if (length(missing_vars)) {
  message("Not set in ~/.Renviron, so not bundled (the app falls back to defaults): ",
          paste(missing_vars, collapse = ", "))
}
env_values <- env_values[nzchar(env_values)]
# if (file.exists(".Renviron")) stop("A project .Renviron already exists; move it before deploying.", call. = FALSE)
writeLines(paste0(names(env_values), "=", env_values), ".Renviron")

tryCatch(
  rsconnect::deployApp(
    appDir = ".",
    appFiles = app_files,
    appName = app_name,
    appTitle = "CFA route comparison",
    forceUpdate = TRUE
  ),
  finally = unlink(".Renviron")
)
