# Pull recent local CFA alerts from the PagerMon site and add any new ones to
# data/cfa_routes.sqlite (the site only keeps about a month, so run this
# regularly, e.g. weekly: 0 7 * * 1 cd /path/to/cfa-routes && Rscript extract_incidents.R).
# Chewton's own pager isn't on this feed, so "local" means a neighbouring brigade
# (or Chewton, if it ever appears) was turned out.
#
# New incidents are then geocoded, routed from each station and given road geometry for the
# map, which needs GOOGLE_MAPS_API_KEY (see run.R); without it this step is skipped and
# run.R can do it later. Run run.R to analyse the results.

for (f in list.files("R", full.names = TRUE)) source(f)

base_url <- "https://eas.tehintartubes.net"
local_codes <- c("CCHEW", "CCAMP", "CCMAI", "CHARC", "CELPH", "CTARA",
                 "CHEW", "CAMP", "CMAI", "HARC", "ELPH", "TARA")
searches <- c("CASTLEMAINE BRIGADE", "HARCOURT", "CAMPBELLS CREEK", "ELPHINSTONE BRIGADE",
              "TARADALE BRIGADE", "CHEWTON BRIGADE")

# Trust this host's TLS certificate even when it is expired or invalid (its Let's Encrypt
# certificate has lapsed before). This applies only to the PagerMon client: the feed is public
# alert text, and Google requests keep full verification.
client <- pagermon_client(base_url, transport = with_retry(
  function(method, url, ...) curl_transport(method, url, ..., insecure = TRUE)
))
# If the feed is unreachable (e.g. a connection failure), stop quietly: nothing new
# can be added, and the data already stored is left alone. The warning shows on the Actions run.
messages <- tryCatch(
  unlist(lapply(searches, function(q) fetch_all_pages(client, q)), recursive = FALSE),
  error = function(e) {
    message("::warning title=PagerMon unreachable::", conditionMessage(e))
    quit(save = "no", status = 0)
  }
)

alerts <- parse_alerts(messages)
local <- alerts[is_local(alerts, local_codes), ]
local$address_guess <- vapply(local$location_text, incident_address, "")

con <- db_connect()

added <- db_add_incidents(con, local)
total <- DBI::dbGetQuery(con, "SELECT COUNT(*) n, MIN(time) oldest FROM incidents")
message(sprintf("%d new incident(s); database now holds %d (oldest %s)",
                added, total$n, format(from_epoch(total$oldest), "%Y-%m-%d")))

# Route anything not yet routed (newest first), then fetch geometry for the map. Each
# incident costs one route request per station; max_incidents caps one run's spend.
max_incidents <- 5
home <- "Chewton"
if (nzchar(Sys.getenv("GOOGLE_MAPS_API_KEY"))) {
  client <- google_client(google_key())
  db_upsert_stations(con, read.csv("config/stations.csv", stringsAsFactors = FALSE))
  routed <- route_pending(con, client, max_incidents, home, speed_factor = 1, with_elevation = FALSE)
  message(length(routed), " incident(s) routed")
  n <- backfill_polylines(con, client, home)
  message(n, " route(s) given road geometry; ", nrow(db_pairs_without_polyline(con, home)), " still without")
} else {
  message("GOOGLE_MAPS_API_KEY not set; skipping routing (run run.R later)")
}

DBI::dbDisconnect(con)
