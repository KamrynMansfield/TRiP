# Find tracts that intersect the route buffer

Find tracts that intersect the route buffer

## Usage

``` r
create_intersecting_tract_percentages(tract_geom, route_geom)
```

## Arguments

- tract_geom:

  the geometry of the census tracts that was output from the
  `get_tract_geometry` function.

- route_geom:

  the route geometry that was output from the `get_gtfs_routes`
  function.

## Value

a data frame with geometries of all the intersections created when
crossing the tract boundaries with the route buffer. It also includes
columns for the percentage of the tract it takes up
