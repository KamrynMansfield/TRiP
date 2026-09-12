# Find EPSG code for State Plane CRS

`get_crs()` uses state plane geometry to find an appropriate NAD83 crs
for the input geometry. This is a copy from the `zr_get_crs()` function
in the zoneR package on
[github](https://github.com/vibe-lab-gsd/zoneR/blob/main/R/zr_get_crs.R)

## Usage

``` r
get_crs(geom_data, large_area = FALSE)
```

## Arguments

- geom_data:

  Either a simple feature collection or a path to a file containing
  geospatial data

- large_area:

  Set this to `TRUE` if your data may cross multiple state planes. It
  will be a bit longer but find the state plane that covers it the best.
  When `FALSE` (the default) only the first non-empty feature is tested,
  which is much faster.

## Value

Returns the appropriate epsg code as an integer

## Details

In TRiP this is called by
[`create_intersecting_tract_percentages()`](https://kamrynmansfield.github.io/TRiP/reference/create_intersecting_tract_percentages.md)
before the quarter-mile route buffer is drawn, because buffering by a
distance in meters requires a projected coordinate system.

The state plane data was compiled using [ArcGIS
Hub](https://hub.arcgis.com/datasets/esri::usa-state-plane-zones-nad83/explore)
and the [epsg.io](https://epsg.io/) website, and ships with the package
as the internal `state_planes_crs` object.

## Examples

``` r
if (FALSE) { # \dontrun{
route_geom <- get_gtfs_routes("gtfs.zip")
get_crs(route_geom)
} # }
```
