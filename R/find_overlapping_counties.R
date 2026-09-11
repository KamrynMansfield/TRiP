# find_overlapping_counties.R ---------------------------------------------
# Step 2 of the geography pipeline: routes -> counties -> tracts -> ACS.
# The counties returned here determine which state/county FIPS codes are sent
# to tigris::tracts() and tidycensus::get_acs().

#' Find the counties that overlap the route geometry.
#'
#' Intersects the agency's route geometry against the internal `us_counties`
#' data set and returns every county the routes touch. The resulting state and
#' county FIPS codes are what [get_tract_geometry()] and [pull_acs_data()] use
#' to limit their Census queries, and the county polygons are also drawn as a
#' grey background layer in [make_route_leaflet()].
#'
#' This uses the county shape data found at
#' [census.gov](https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html)
#'
#' @param route_geom The route geometry that was created with `get_gtfs_routes()`.
#'   Expected to be an `sf` object in EPSG:4326 (WGS 84).
#'
#' @returns An `sf` data frame of the intersecting counties, including the
#'   columns "STATEFP", "COUNTYFP", and "NAME".
#' @export
#'
#' @examples
#' \dontrun{
#' route_geom <- get_gtfs_routes("gtfs.zip")
#' county_sf  <- find_overlapping_counties(route_geom)
#' }
find_overlapping_counties <- function(route_geom){

  county_sf <- us_counties # internal package data (see data-raw/ and sysdata.rda)

  # st_intersects() returns a list: one element per route, each holding the row
  # indexes of every county that route crosses. Flatten that list and de-duplicate
  # so each county appears only once.
  idxs <- st_intersects(route_geom, st_transform(county_sf, 4326)) |>
    unique() |>
    unlist() |>
    unique()

  # subset the county layer down to just the intersecting rows
  final_df <- county_sf[idxs,]

  return(final_df)
}

# R <- find_overlapping_counties(route_geom)
