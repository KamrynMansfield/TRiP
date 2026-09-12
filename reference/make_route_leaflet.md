# Build the interactive route map

Draws the agency's bus routes over a light basemap, with the overlapping
county boundaries underneath for context. Each route gets its own color,
and hovering over a route highlights it in red and shows its `route_id`
as a tooltip so the user can confirm the IDs match their ridership file.

## Usage

``` r
make_route_leaflet(routes_sf, county_sf)
```

## Arguments

- routes_sf:

  An `sf` object of route geometries with a `route_id` column, as
  returned by
  [`get_gtfs_routes()`](https://kamrynmansfield.github.io/TRiP/reference/get_gtfs_routes.md).

- county_sf:

  An `sf` object of county polygons, as returned by
  [`find_overlapping_counties()`](https://kamrynmansfield.github.io/TRiP/reference/find_overlapping_counties.md),
  drawn as a grey reference layer.

## Value

A `leaflet` htmlwidget, rendered by `renderLeaflet()` in
[`app_server()`](https://kamrynmansfield.github.io/TRiP/reference/app_server.md).

## Examples

``` r
if (FALSE) { # \dontrun{
routes_sf <- get_gtfs_routes("gtfs.zip")
make_route_leaflet(routes_sf, find_overlapping_counties(routes_sf))
} # }
```
