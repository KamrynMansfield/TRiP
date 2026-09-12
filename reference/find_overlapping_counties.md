# Find the counties that overlap the route geometry.

Intersects the agency's route geometry against the internal
`us_counties` data set and returns every county the routes touch. The
resulting state and county FIPS codes are what
[`get_tract_geometry()`](https://kamrynmansfield.github.io/TRiP/reference/get_tract_geometry.md)
and
[`pull_acs_data()`](https://kamrynmansfield.github.io/TRiP/reference/pull_acs_data.md)
use to limit their Census queries, and the county polygons are also
drawn as a grey background layer in
[`make_route_leaflet()`](https://kamrynmansfield.github.io/TRiP/reference/make_route_leaflet.md).

## Usage

``` r
find_overlapping_counties(route_geom)
```

## Arguments

- route_geom:

  The route geometry that was created with
  [`get_gtfs_routes()`](https://kamrynmansfield.github.io/TRiP/reference/get_gtfs_routes.md).
  Expected to be an `sf` object in EPSG:4326 (WGS 84).

## Value

An `sf` data frame of the intersecting counties, including the columns
"STATEFP", "COUNTYFP", and "NAME".

## Details

This uses the county shape data found at
[census.gov](https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html)

## Examples

``` r
if (FALSE) { # \dontrun{
route_geom <- get_gtfs_routes("gtfs.zip")
county_sf  <- find_overlapping_counties(route_geom)
} # }
```
