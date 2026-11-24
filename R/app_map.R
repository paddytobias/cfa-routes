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

station_map <- function(stations) {
  leaflet::leaflet() |>
    leaflet::fitBounds(min(stations$lng) - 0.05, min(stations$lat) - 0.05,
                       max(stations$lng) + 0.05, max(stations$lat) + 0.05) |>
    add_basemap() |>
    leaflet::addCircleMarkers(data = stations, lng = ~lng, lat = ~lat, radius = 9, weight = 3,
                              color = "#000000", fillColor = "#FFFFFF", fillOpacity = 1,
                              label = ~station, labelOptions = leaflet::labelOptions(permanent = TRUE, direction = "top"),
                              group = "stations")
}

base_map <- function(stations) {
  station_map(stations) |>
    leaflet::addLegend("bottomright", colors = unname(verdict_colours), labels = names(verdict_colours),
                       title = "Incident: Chewton vs paged", opacity = 1)
}

zone_colours <- c("First" = "#0072B2", "Second" = "#E69F00", "Third" = "#009E73",
                  "Fourth or later" = "#D9D9D9", "No data" = "#FFFFFF")

incident_label <- function(i) {
  sprintf("<b>%s</b><br>%s<br>Paged: %s", format(i$event_time, "%a %d %b %H:%M"),
          htmltools::htmlEscape(i$incident_address), htmltools::htmlEscape(i$paged))
}

# Panes stack in z-index order: the zone squares sit below the station and incident markers
# (the default overlay pane, 400), and the turnout area outline (from read_turnout_area()) sits
# above them, so neither the squares nor the outline hide anything. The incident points, from
# db_read_incidents(), are a toggle in the layers control and are shown to begin with.
zone_map <- function(stations, home = "Chewton", turnout_area = NULL, incidents = NULL) {
  shown <- names(zone_colours) != "No data"
  map <- station_map(stations) |> leaflet::addMapPane("zone_tiles", zIndex = 380)
  if (!is.null(turnout_area)) {
    map <- map |>
      leaflet::addMapPane("turnout_area", zIndex = 450) |>
      leaflet::addGeoJSON(turnout_area$geojson, group = "turnout_area", fill = FALSE, color = "#000000",
                          weight = 3, opacity = 1, dashArray = "8 6",
                          options = leaflet::pathOptions(pane = "turnout_area", interactive = FALSE))
  }
  incidents <- incidents[!is.na(incidents$lat) & !is.na(incidents$lng), ]
  if (!is.null(incidents) && nrow(incidents) > 0) {
    map <- map |>
      leaflet::addCircleMarkers(data = incidents, lng = ~lng, lat = ~lat, radius = 5, weight = 1.5,
                                color = "#FFFFFF", fillColor = "#000000", fillOpacity = 1,
                                popup = incident_label(incidents), group = "Incidents") |>
      leaflet::addLayersControl(overlayGroups = "Incidents", position = "topright",
                                options = leaflet::layersControlOptions(collapsed = FALSE))
  }
  map |>
    leaflet::addLegend("bottomright", colors = unname(zone_colours[shown]),
                       labels = sprintf("%s %s", home, tolower(names(zone_colours)[shown])),
                       title = "Arrives", opacity = 1)
}

# Ring the selected incident (one row of db_read_incidents(), or none to clear). It is a separate
# group from the incident points, so the toggle in the layers control doesn't affect it, and it is
# added after them, so it sits on top.
highlight_incident <- function(map, incident) {
  map <- leaflet::clearGroup(map, "selected_incident")
  if (is.null(incident) || nrow(incident) == 0 || is.na(incident$lat[1]) || is.na(incident$lng[1])) return(map)
  leaflet::addCircleMarkers(map, lng = incident$lng[1], lat = incident$lat[1], radius = 12, weight = 4,
                            color = "#D4006A", fill = FALSE, opacity = 1, group = "selected_incident",
                            popup = incident_label(incident[1, ]))
}

zone_popup <- function(z, home = "Chewton") {
  sprintf("%s arrives <b>%s</b> (%s min)<br>First: %s (%s min)<br>Second: %s (%s min)",
          home, tolower(z$zone), ifelse(is.na(z$home_eta), "n/a", sprintf("%.1f", z$home_eta)),
          z$first, sprintf("%.1f", z$first_eta), ifelse(is.na(z$second), "n/a", z$second),
          ifelse(is.na(z$second_eta), "n/a", sprintf("%.1f", z$second_eta)))
}

# Replace the zone tiles. `zones` is rank_cells() output and `bounds` cell_bounds(); cells
# whose lead is within the margin get a dark outline when `outline_contested` is set.
add_zone_cells <- function(map, zones, bounds, outline_contested = TRUE, home = "Chewton") {
  map <- leaflet::clearGroup(map, "zones")
  if (is.null(zones) || nrow(zones) == 0) return(map)
  d <- merge(zones, bounds, by = "cell_id")
  d <- d[d$zone != "No data", ]
  outline <- outline_contested & d$contested
  leaflet::addRectangles(map, data = d, lng1 = ~lng1, lat1 = ~lat1, lng2 = ~lng2, lat2 = ~lat2,
                         group = "zones", fillColor = unname(zone_colours[as.character(d$zone)]), fillOpacity = 0.6,
                         color = "#000000", weight = ifelse(outline, 2, 0), opacity = 1, popup = zone_popup(d, home),
                         options = leaflet::pathOptions(pane = "zone_tiles"))
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
