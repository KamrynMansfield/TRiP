#' Find the counties that overlap the route geometry.
#'
#' This uses the county shape data found at
#' [census.gov](https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html)
#'
#' @param route_geom The route geometry that was created with `get_gtfs_routes()`
#'
#' @returns A data frame with the following columns, "STATEFP", "COUNTYFP", "NAME"
#' @export
#'
#' @examples
find_overlapping_counties <- function(route_geom){

  county_sf <- us_counties

  idxs <- st_intersects(route_geom, st_transform(county_sf, 4326)) |>
    unique() |>
    unlist() |>
    unique()

  final_df <- county_sf[idxs,]

  return(final_df)
}

# R <- find_overlapping_counties(route_geom)
