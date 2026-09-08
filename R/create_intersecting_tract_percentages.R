#' Find tracts that intersect the route buffer
#'
#' @param tract_geom the geometry of the census tracts that
#' was output from the `get_tract_geometry` function.
#' @param route_geom the route geometry that was output
#' from the `get_gtfs_routes` function. 
#'
#' @returns a data frame with geometries of all the intersections created
#' when crossing the tract boundaries with the route buffer. It 
#' also includes columns for the percentage of the tract it takes up
#' @export
#'
#' @examples

# tract_geom <- census_tract_geom

create_intersecting_tract_percentages <- function(tract_geom, route_geom){

  ## Creating a buffer around the routes
  # finding the appropriate state plane coordinate reference system
  crs <- get_crs(route_geom)

  sf_use_s2(TRUE)
  
  route_simple <- st_simplify(route_geom, dTolerance = 2) |>
    st_transform(crs)
  
  # use planar geometry since we are using a projected coordinate system
  sf_use_s2(FALSE)
  
  # create a 1/4 mile buffer around each route
  shapes_buffered <- route_simple |>
    st_buffer(dist = set_units(402.336, "m"))
  
  # Area of each buffer segment
  shapes_buffered$buffer_area <- st_area(shapes_buffered)

  years <- unique(tract_geom$year)
  
  tract_geom <- st_transform(tract_geom, crs)

  intersections_list <- list() # empty list to store data frames
  # loop through each year to make it's own data
  for (year in years){
    year_val <- year
    tracts <- tract_geom |>
      filter(year == year_val) |>
      select(GEOID, geometry)

    # Get indexes of intersecting tracts
    intersections <- st_intersects(tracts, shapes_buffered)

    # Keep only those that intersect
    tracts_touching <- tracts[lengths(intersections) > 0, ]

    # Reproject to a projected CRS (e.g., NAD83 / Conus Albers)
    # target_crs <- 5070  # EPSG:5070 (USA Contiguous Albers Equal Area)
    # tracts_touching <- st_transform(tracts_touching, target_crs)
    # shapes_buffered <- st_transform(shapes_buffered, target_crs)

    # Perform intersection between tracts and buffered shapes
    intersections <- st_intersection(tracts_touching, shapes_buffered)

    # Area of each intersected piece (tract + buffer overlap)
    intersections$intersect_area <- st_area(intersections)

    # calculate % of each route's buffer in each tract
    intersections$percent_of_buffer <- as.numeric(
      intersections$intersect_area / intersections$buffer_area
    )

    # add a column for the year
    intersections$year <- year
    intersections_list[[as.character(year)]] <- intersections

  }
  
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