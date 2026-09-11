# create_intersecting_tract_percentages.R ---------------------------------
# Step 6 of the pipeline: work out how much of each route's quarter-mile
# catchment area falls inside each census tract. Those shares become the
# weights that turn tract-level ACS data into route-level ACS data in
# create_final_acs_data().

#' Find tracts that intersect the route buffer
#'
#' Draws a quarter-mile (402.336 m) buffer around each route, overlays it with
#' the census tracts for each year, and computes what share of the route's
#' buffer falls in each tract. Those shares are the weights used later to
#' aggregate tract demographics up to the route level.
#'
#' The route geometry is simplified and reprojected to a local state plane CRS
#' first, so the buffer distance and areas are computed in meters on a planar
#' surface rather than in degrees on a sphere.
#'
#' @param tract_geom the geometry of the census tracts that
#' was output from the `get_tract_geometry` function. Must include a `year`
#' column; the overlay is repeated once per year because tract boundaries change
#' between vintages.
#' @param route_geom the route geometry that was output
#' from the `get_gtfs_routes` function.
#'
#' @returns An `sf` data frame (EPSG:4326) with one row per route-tract-year
#'   overlap, holding the geometry of the overlap plus:
#'   \describe{
#'     \item{GEOID}{The census tract identifier.}
#'     \item{route_id}{The route whose buffer produced the overlap.}
#'     \item{buffer_area}{Total area of that route's buffer.}
#'     \item{intersect_area}{Area of this particular route-tract overlap.}
#'     \item{percent_of_buffer}{`intersect_area / buffer_area`, the weight used downstream.}
#'     \item{year}{The tract vintage the overlap was computed against.}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' tract_buffer_data <- create_intersecting_tract_percentages(
#'   census_tract_geom, route_geom
#' )
#' }

# tract_geom <- census_tract_geom

create_intersecting_tract_percentages <- function(tract_geom, route_geom){

  ## Creating a buffer around the routes
  # finding the appropriate state plane coordinate reference system
  crs <- get_crs(route_geom)

  # s2 spherical geometry on for the simplify step (data is still lat/long here)
  sf_use_s2(TRUE)

  # thinning the route vertices dramatically speeds up buffering and
  # intersection for large agencies, at negligible cost to accuracy
  route_simple <- st_simplify(route_geom, dTolerance = 2) |>
    st_transform(crs)

  # use planar geometry since we are using a projected coordinate system
  sf_use_s2(FALSE)

  # create a 1/4 mile buffer around each route
  # (402.336 m == 0.25 mi; units are meaningful now that the CRS is projected)
  shapes_buffered <- route_simple |>
    st_buffer(dist = set_units(402.336, "m"))

  # Area of each buffer segment
  # this is the denominator for percent_of_buffer below
  shapes_buffered$buffer_area <- st_area(shapes_buffered)

  years <- unique(tract_geom$year)

  # put the tracts in the same projected CRS as the buffers
  tract_geom <- st_transform(tract_geom, crs)

  intersections_list <- list() # empty list to store data frames
  # loop through each year to make it's own data
  # (tract boundaries differ by vintage, so each year is overlaid separately)
  for (year in years){
    year_val <- year # alias avoids the filter() masking the loop variable
    tracts <- tract_geom |>
      filter(year == year_val) |>
      select(GEOID, geometry)

    # Get indexes of intersecting tracts
    intersections <- st_intersects(tracts, shapes_buffered)

    # Keep only those that intersect
    # (prefiltering makes the expensive st_intersection() call much cheaper)
    tracts_touching <- tracts[lengths(intersections) > 0, ]

    # Reproject to a projected CRS (e.g., NAD83 / Conus Albers)
    # target_crs <- 5070  # EPSG:5070 (USA Contiguous Albers Equal Area)
    # tracts_touching <- st_transform(tracts_touching, target_crs)
    # shapes_buffered <- st_transform(shapes_buffered, target_crs)

    # Perform intersection between tracts and buffered shapes
    # this returns one row per tract/route overlap, carrying both sets of columns
    intersections <- st_intersection(tracts_touching, shapes_buffered)

    # Area of each intersected piece (tract + buffer overlap)
    intersections$intersect_area <- st_area(intersections)

    # calculate % of each route's buffer in each tract
    # these weights sum to roughly 1 across all tracts for a given route
    intersections$percent_of_buffer <- as.numeric(
      intersections$intersect_area / intersections$buffer_area
    )

    # add a column for the year
    intersections$year <- year
    intersections_list[[as.character(year)]] <- intersections

  }

  # stack every year and return to lat/long for mapping and joining
  combined_intersections <- bind_rows(intersections_list) |>
    st_transform(4326)

  return(combined_intersections)
}


## This was just me creating some plots to see how the atlanta
## data is much quicker when it is smaller
#
# r115 <- route_geom[115,]
#
# coords <- st_coordinates(r115)
# point_coords <- c(coords[[1,1]], coords[[1,2]])
# st_point(point_coords)
# points_list <- list(st_point(point_coords), st_point(point_coords))
#
#
# points_list <- list()
# for (i in 1:nrow(coords)){
#   point_coords <- c(coords[[i,1]], coords[[i,2]])
#   points_list[[i]] <- st_point(point_coords)
# }
#
# r115_points <- st_as_sf(data.frame(id = 1:nrow(coords)),geometry = points_list, crs = 4326)
#
#
# ggplot() +
#   geom_sf(data = r115, color ="black") +
#   geom_sf(data = r115_points, color ="red") +
#   theme_void()
#
#
# r115_simplified <- route_geom[115,] |>
#   st_simplify(dTolerance = 2)
#
# coords <- st_coordinates(r115_simplified)
# point_coords <- c(coords[[1,1]], coords[[1,2]])
# st_point(point_coords)
# points_list <- list(st_point(point_coords), st_point(point_coords))
#
#
# points_list <- list()
# for (i in 1:nrow(coords)){
#   point_coords <- c(coords[[i,1]], coords[[i,2]])
#   points_list[[i]] <- st_point(point_coords)
# }
#
# r115_simplified_points <- st_as_sf(data.frame(id = 1:nrow(coords)),geometry = points_list, crs = 4326)
#
#
# ggplot() +
#   geom_sf(data = r115, color ="black") +
#   geom_sf(data = r115_simplified_points, color ="red") +
#   theme_void()
#
