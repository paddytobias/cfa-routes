# Grid of points covering the district, for working out which brigade arrives
# first and second everywhere, not just at past incidents. Pure; no I/O.

km_per_degree <- 111.195

# Cell centres within `radius_km` of (lat, lng), `spacing_km` apart. Each id encodes
# the cell's row and column offset from the centre, so it stays the same if the
# radius changes later and cells already fetched are kept.
make_grid <- function(lat, lng, radius_km = 25, spacing_km = 1) {
  steps <- seq.int(-floor(radius_km / spacing_km), floor(radius_km / spacing_km))
  cells <- expand.grid(row = steps, col = steps)
  cells$lat <- lat + cells$row * spacing_km / km_per_degree
  cells$lng <- lng + cells$col * spacing_km / (km_per_degree * cos(lat * pi / 180))
  cells$dist_km <- haversine_m(lat, lng, cells$lat, cells$lng) / 1000
  cells <- cells[cells$dist_km <= radius_km, ]
  cells$cell_id <- sprintf("r%+03d_c%+03d", cells$row, cells$col)
  cells <- cells[order(cells$dist_km, cells$cell_id), c("cell_id", "lat", "lng", "dist_km")]
  rownames(cells) <- NULL
  cells
}

# Grid spacing recovered from the cell centres, so the app needs no extra setting.
grid_spacing_km <- function(cells) {
  d <- diff(sort(unique(round(cells$lat, 6))))
  median(d[d > 0]) * km_per_degree
}

# Rectangle edges for drawing each cell as a tile.
cell_bounds <- function(cells, spacing_km) {
  half_lat <- spacing_km / 2 / km_per_degree
  half_lng <- spacing_km / 2 / (km_per_degree * cos(cells$lat * pi / 180))
  data.frame(cell_id = cells$cell_id, lat1 = cells$lat - half_lat, lat2 = cells$lat + half_lat,
             lng1 = cells$lng - half_lng, lng2 = cells$lng + half_lng, stringsAsFactors = FALSE)
}
