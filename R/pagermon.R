# Incident extraction from a PagerMon instance. The transport is the only
# impure piece; parsing and filtering are pure.

pagermon_client <- function(base_url, transport = with_retry(curl_transport)) {
  force(base_url); force(transport)
  list(
    search = function(q, page = 1L) transport(
      "GET", sprintf("%s/api/messageSearch?q=%s&page=%d", base_url, curl::curl_escape(q), page)
    )
  )
}

# Fetch every page of a search. `pause` keeps us polite to a volunteer-run server.
fetch_all_pages <- function(client, q, max_pages = 20L, pause = 0.5) {
  fetch_page <- function(page) {
    if (page > 1L) Sys.sleep(pause)
    client$search(q, page)
  }
  first <- fetch_page(1L)
  last <- min(first$init$pageCount, max_pages)
  rest <- lapply(seq_len(max(last - 1L, 0L)) + 1L, fetch_page)
  unlist(lapply(c(list(first), rest), `[[`, "messages"), recursive = FALSE)
}

# ---- Pure parsing ------------------------------------------------------------

# Primary alert: "@@ALERT CMAI6 G&SC1 <description + address> <map ref> (<grid>) F <turnout...> <event>"
alert_rx <- paste0(
  "^@@ALERT ([A-Z]+)(\\d*) ([A-Z&]+\\d?) (.+?) ",
  "((?:SVC|SVNW|SVNE|SVSE|SVSW|M) \\d+ [A-Z]\\d+) \\((\\d+)\\) ",
  "(?:F )?(.*?)\\s*(F\\d{9})$"
)

# Returns one row, or NULL if the message isn't a parseable CFA alert. Anything
# that isn't an alert (chatter, SES jobs with caller phone numbers) is dropped
# here, so personal details never reach the output.
parse_alert <- function(m) {
  text <- sub("\\s*\\[[A-Z0-9_]+\\]$", "", m$message)
  hit <- regmatches(text, regexec(alert_rx, text, perl = TRUE))[[1]]
  if (length(hit) == 0) return(NULL)
  data.frame(
    event = hit[9],
    time = as.POSIXct(m$timestamp, origin = "1970-01-01", tz = "Australia/Melbourne"),
    primary_brigade = hit[2],
    response_code = hit[4],
    location_text = hit[5],
    map_ref = hit[6],
    turnout = hit[8],
    stringsAsFactors = FALSE
  )
}

# One row per event (the same alert is paged to every responding brigade).
parse_alerts <- function(messages) {
  rows <- Filter(Negate(is.null), lapply(messages, parse_alert))
  if (length(rows) == 0) return(NULL)
  alerts <- do.call(rbind, rows)
  alerts <- alerts[!duplicated(alerts$event), ]
  alerts[order(alerts$time, decreasing = TRUE), ]
}

# "Local" = any of the given pager codes is in the turnout, or the primary brigade is.
is_local <- function(alerts, codes) {
  turned_out <- strsplit(alerts$turnout, "\\s+")
  vapply(seq_len(nrow(alerts)), function(i) {
    any(c(alerts$primary_brigade[i], turned_out[[i]]) %in% codes)
  }, logical(1))
}

# Best-effort street address from an alert's free text (description + address +
# cross streets, no delimiters). Prefers "<number> <street> <locality>", then
# "CNR a/b <locality>", else the whole text. Review before trusting the geocode.
incident_address <- function(location_text) {
  text <- sub("\\s/.*$", "", location_text)
  numbered <- regmatches(text, regexpr("(?<![\\w:.,-])\\d+[A-Z]?(?:-\\d+)?\\s+[A-Z][A-Z' ]*$", text, perl = TRUE))
  corner <- regmatches(text, regexpr("\\bCNR .*$", text, perl = TRUE))
  best <- if (length(numbered)) numbered else if (length(corner)) corner else text
  paste0(best, ", VIC, Australia")
}

# Shape the alerts like config/incidents.csv so they can feed compare_brigades().
incidents_config <- function(alerts) {
  data.frame(
    incident = alerts$event,
    address = vapply(alerts$location_text, incident_address, ""),
    lat = NA_real_, lng = NA_real_,
    event_time = alerts$time,
    incident_address = alerts$location_text,
    paged = format_paged(alerts$turnout),
    row.names = NULL
  )
}

# ---- Archive ------------------------------------------------------------------

# One-off: read the old CSV archive so it can be imported into the database.
time_format <- "%Y-%m-%d %H:%M:%S"

read_archive <- function(path, tz = "Australia/Melbourne") {
  if (!file.exists(path)) return(NULL)
  archive <- read.csv(path, stringsAsFactors = FALSE)
  archive$time <- as.POSIXct(archive$time, format = time_format, tz = tz)
  archive
}
