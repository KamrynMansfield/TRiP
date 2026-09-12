# Get transit route geometry

Reads a GTFS feed and collapses it into a single line feature per route.
GTFS stores geometry in `shapes.txt` keyed by `shape_id`, and a route
typically has many shape IDs (one per pattern and direction), so this
function joins shapes to routes through `trips.txt` and then unions all
the shapes belonging to each route.

## Usage

``` r
get_gtfs_routes(gtfs_zip_file)
```

## Arguments

- gtfs_zip_file:

  Path to the agency's GTFS zip file.

## Value

An `sf` data frame with one row per route, containing a `route_id`
column and the unioned line geometry for that route, in EPSG:4326. The
`route_id` values must match the `route_id` values in the uploaded
ridership spreadsheet;
[`app_server()`](https://kamrynmansfield.github.io/TRiP/reference/app_server.md)
checks this and blocks the user if they disagree.

## Examples

``` r
if (FALSE) { # \dontrun{
route_geom <- get_gtfs_routes("knoxville_gtfs.zip")
} # }
```
