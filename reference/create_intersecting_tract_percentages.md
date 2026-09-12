# Find tracts that intersect the route buffer

Draws a quarter-mile (402.336 m) buffer around each route, overlays it
with the census tracts for each year, and computes what share of the
route's buffer falls in each tract. Those shares are the weights used
later to aggregate tract demographics up to the route level.

## Usage

``` r
create_intersecting_tract_percentages(tract_geom, route_geom)
```

## Arguments

- tract_geom:

  the geometry of the census tracts that was output from the
  `get_tract_geometry` function. Must include a `year` column; the
  overlay is repeated once per year because tract boundaries change
  between vintages.

- route_geom:

  the route geometry that was output from the `get_gtfs_routes`
  function.

## Value

An `sf` data frame (EPSG:4326) with one row per route-tract-year
overlap, holding the geometry of the overlap plus:

- GEOID:

  The census tract identifier.

- route_id:

  The route whose buffer produced the overlap.

- buffer_area:

  Total area of that route's buffer.

- intersect_area:

  Area of this particular route-tract overlap.

- percent_of_buffer:

  `intersect_area / buffer_area`, the weight used downstream.

- year:

  The tract vintage the overlap was computed against.

## Details

The route geometry is simplified and reprojected to a local state plane
CRS first, so the buffer distance and areas are computed in meters on a
planar surface rather than in degrees on a sphere.

## Examples

``` r
if (FALSE) { # \dontrun{
tract_buffer_data <- create_intersecting_tract_percentages(
  census_tract_geom, route_geom
)
} # }
```
