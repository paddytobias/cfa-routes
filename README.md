# CFA route comparison

Would Chewton CFA have reached local incidents faster than the brigades that were actually paged? This Shiny app compares estimated road travel times from each station to each incident and shows where Chewton would have been first.

**Live app: <https://paddytobias.shinyapps.io/cfa-routes/>**

## How it works

1. `extract_incidents.R` pulls recent alerts for Chewton and its neighbouring brigades (Castlemaine, Campbells Creek, Harcourt, Elphinstone, Taradale, Fryerstown) from a [PagerMon](https://github.com/pagermon/pagermon) feed and stores new incidents in a SQLite database.
2. New incidents and stations are geocoded, then routed from every station with the Google Maps Routes API, which supplies predicted traffic-aware travel times and road geometry.
3. The app reads the database and, for each incident, compares Chewton's estimated arrival with the first paged brigade.

The app lets you adjust:

- extra turnout time for Chewton
- the margin that counts as "within X minutes" of the first brigade
- a speed factor for all brigades, since Google's times are for ordinary cars

It shows headline figures, a map of the routes, a sortable table of all incidents and a CSV download.

> [!NOTE]
> Times are Google's predicted car travel times for the same weekday and time next week. They exclude turnout and don't reflect lights and sirens. Incidents where only Chewton was paged aren't in the feed, so the data is not a random sample of the district. The app's About tab lists the full set of limitations.

## Run locally

Requires R and the packages in `renv.lock`.

```sh
Rscript -e 'renv::restore()'
Rscript -e 'shiny::runApp()'
```

The app reads `data/cfa_routes.sqlite`, or the file named in the `CFA_DB` environment variable.

### Update the data

Add `GOOGLE_MAPS_API_KEY=<your key>` to `~/.Renviron`, with the Routes, Geocoding and Elevation APIs enabled on the key's project.

```sh
Rscript extract_incidents.R   # fetch, geocode and route new incidents
Rscript run.R                 # route pending incidents and write results/route_comparison.csv
```

### Turnout zones

The **Turnout zones** tab maps where Chewton would arrive first, second or later across the district, using the speed factor, Chewton turnout time and margin from the sidebar, plus a "brigades unable to respond" scenario. It needs travel times from every station to a 1 km grid within 25 km of Chewton:

```sh
Rscript fetch_grid.R         # pilot: 600 station/cell pairs, nearest Chewton first
Rscript fetch_grid.R 20000   # the rest (about 12,000 pairs in all); re-running resumes
```

Google bills per station/cell pair, so check the charge after the pilot. To add a brigade, add it to `config/stations.csv` and re-run: only its column is fetched. The map outlines Chewton's primary turnout area from `data/turnout_area.geojson`, loaded at startup; replace that file to change it. Past incidents are shown as black points that can be switched on and off from the layers control on the map. The sidebar's incident picker rings the chosen incident.

If you move a station, routes and grid times measured from its old position are stale. Changing its `lat`/`lng` in `config/stations.csv` is picked up automatically. If you edit the `stations` table in the database directly, name the station so its results are cleared and fetched again (this bills Google for about 2,000 grid cells per station plus one route per incident):

```sh
Rscript refresh_stations.R Maldon "Sutton Grange"
Rscript refresh_stations.R --no-fetch Maldon   # only clear; fetch later
```

### Comparing with r5r

`compare_r5r.R` measures how close travel times from [r5r](https://ipeagit.github.io/r5r/) (routing on OpenStreetMap, no API charges) are to the Google times stored in the database, for the incident routes and the zones grid, and how often the two put Chewton in the same turnout zone. It writes nothing to the database. It needs JDK 21, `osmium` and the R packages `r5r` and `data.table`, which are deliberately not listed in `DESCRIPTION` so the deployed app doesn't install them. The first run downloads Victoria's road data (about 250 MB) into `osm/`.

```sh
Rscript compare_r5r.R
Rscript tests/test_compare.R   # offline tests for the helpers
```

Optionally set `CARTO_BASEMAP_URL` for keyed Carto map tiles; the app falls back to the free tiles without it.

### Tests

The tests need no network access:

```sh
Rscript tests/test_pure.R
Rscript tests/test_db.R
Rscript tests/test_case.R
Rscript tests/test_app.R
Rscript tests/test_zones.R
Rscript tests/test_compare.R
```

## Deployment

A [GitHub Actions workflow](.github/workflows/daily-update.yml) runs daily at 07:00 Melbourne time. It extracts and routes new incidents, commits the updated database, and redeploys to shinyapps.io with `deploy.R`. To deploy by hand, register your shinyapps.io credentials as described at the top of `deploy.R`, then run `source("deploy.R")`.

The workflow needs these repository secrets: `GOOGLE_MAPS_API_KEY`, `CARTO_BASEMAP_URL`, `SHINYAPPS_NAME`, `SHINYAPPS_TOKEN` and `SHINYAPPS_SECRET`. The Google key is never deployed with the app.

## Project layout

| Path | Purpose |
| --- | --- |
| `app.R` | Shiny entry point |
| `R/` | App UI and server, database, Google and PagerMon clients, analysis |
| `compare_r5r.R` | Compare r5r travel times with the stored Google times |
| `refresh_stations.R` | Clear and refetch the results of stations whose coordinates changed |
| `fetch_grid.R` | Fetch station-to-grid travel times for the turnout zones map |
| `config/stations.csv` | Stations and their pager codes |
| `data/cfa_routes.sqlite` | Stored incidents, routes and geometry |
| `data/turnout_area.geojson` | Chewton's primary turnout area, outlined on the Turnout zones map |
| `tests/` | Offline tests |
