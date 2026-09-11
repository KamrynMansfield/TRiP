# Find the counties that overlap the route geometry.

This uses the county shape data found at
[census.gov](https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html)

## Usage

``` r
find_overlapping_counties(route_geom)
```

## Arguments

- route_geom:

  The route geometry that was created with
  [`get_gtfs_routes()`](https://kamrynmansfield.github.io/TRiP/reference/get_gtfs_routes.md)

## Value

A data frame with the following columns, "STATEFP", "COUNTYFP", "NAME"
