# Leaflet building blocks for the app. They take data and return map objects.

verdict_colours <- c(
  "Chewton first" = "#0072B2", "Within margin" = "#56B4E9", "Ahead of some paged" = "#E69F00",
  "Slower than all paged" = "#D55E00", "Chewton paged" = "#999999", "No comparison" = "#CCCCCC"
)

incident_popup <- function(v) {
  sprintf("<b>%s</b><br>%s<br>Paged: %s<br>Chewton ETA: %s min<br>First paged: %s<br>Verdict: <b>%s</b>",
          format(v$event_time, "%a %d %b %H:%M"), v$incident_address, v$paged,
          ifelse(is.na(v$chewton_eta), "n/a", sprintf("%.1f", v$chewton_eta)),
          ifelse(is.na(v$first_paged), "n/a", sprintf("%s %.1f min", v$first_paged, v$first_paged_eta)),
          v$verdict)
}

# Keyed Carto tiles when CARTO_BASEMAP_URL is set, else the free Positron tiles.
add_basemap <- function(map) {
  url <- Sys.getenv("CARTO_BASEMAP_URL")
  if (!nzchar(url)) return(leaflet::addProviderTiles(map, leaflet::providers$CartoDB.Positron))
  leaflet::addTiles(map, urlTemplate = url, attribution = "&copy; OpenStreetMap contributors &copy; CARTO")
}

base_map <- function(stations) {
  leaflet::leaflet() |>
    leaflet::fitBounds(min(stations$lng) - 0.05, min(stations$lat) - 0.05,
                       max(stations$lng) + 0.05, max(stations$lat) + 0.05) |>
    add_basemap() |>
    leaflet::addCircleMarkers(data = stations, lng = ~lng, lat = ~lat, radius = 9, weight = 3,
                              color = "#000000", fillColor = "#FFFFFF", fillOpacity = 1,
                              label = ~station, labelOptions = leaflet::labelOptions(permanent = TRUE, direction = "top"),
                              group = "stations") |>
    leaflet::addLegend("bottomright", colors = unname(verdict_colours), labels = names(verdict_colours),
                       title = "Incident: Chewton vs paged", opacity = 1)
}

# Replace the incident markers (the map's zoom and the selected routes are untouched).
add_incident_markers <- function(map, verdicts, points) {
  map <- leaflet::clearGroup(map, "incidents")
  if (is.null(verdicts) || nrow(verdicts) == 0) return(map)
  d <- merge(verdicts, points[c("incident", "lat", "lng")], by = "incident")
  leaflet::addCircleMarkers(map, data = d, lng = ~lng, lat = ~lat, layerId = ~incident, radius = 7,
                            weight = 1, color = "#333333", fillColor = unname(verdict_colours[as.character(d$verdict)]),
                            fillOpacity = 0.9, popup = incident_popup(d), group = "incidents")
}

# Draw route_paths() output: Chewton thick red, paged brigades blue, straight fallbacks dashed.
add_routes <- function(map, paths) {
  map <- leaflet::clearGroup(map, "routes")
  draw <- function(m, p) {
    chewton <- p$role == "chewton"
    leaflet::addPolylines(m, data = p$points, lng = ~lng, lat = ~lat, group = "routes",
                          color = if (chewton) "#B30000" else "#1F5FBF", weight = if (chewton) 5 else 3,
                          opacity = 0.85, dashArray = if (p$straight) "6 8" else NULL, label = p$station)
  }
  Reduce(draw, paths, map)
}
