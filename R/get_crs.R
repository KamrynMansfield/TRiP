# get_crs.R ---------------------------------------------------------------
# Picks a projected CRS for the route geometry so that buffering and area
# calculations in create_intersecting_tract_percentages() happen in meters
# rather than degrees.

#' Find EPSG code for State Plane CRS
#'
#' `get_crs()` uses state plane geometry to find an appropriate NAD83
#' crs for the input geometry. This is a copy from the `zr_get_crs()` function in
#' the zoneR package on [github](https://github.com/vibe-lab-gsd/zoneR/blob/main/R/zr_get_crs.R)
#'
#' In TRiP this is called by [create_intersecting_tract_percentages()] before
#' the quarter-mile route buffer is drawn, because buffering by a distance in
#' meters requires a projected coordinate system.
#'
#' @details
#' The state plane data was compiled using [ArcGIS Hub](https://hub.arcgis.com/datasets/esri::usa-state-plane-zones-nad83/explore)
#' and the [epsg.io](https://epsg.io/) website, and ships with the package as the
#' internal `state_planes_crs` object.
#'
#' @param geom_data Either a simple feature collection or a
#' path to a file containing geospatial data
#' @param large_area Set this to `TRUE` if your data may
#' cross multiple state planes. It will be a bit longer but
#' find the state plane that covers it the best. When `FALSE` (the default)
#' only the first non-empty feature is tested, which is much faster.
#'
#' @returns Returns the appropriate epsg code as an integer
#' @export
#'
#' @examples
#' \dontrun{
#' route_geom <- get_gtfs_routes("gtfs.zip")
#' get_crs(route_geom)
#' }
#'
get_crs <- function(geom_data, large_area = FALSE){

  # find out what type of data was input
  if (inherits(geom_data, "sf")){ # it is already an sf object
    geom <- geom_data
  } else if (inherits(geom_data, "character")){ # it might be a file path
    if (file.exists(geom_data)){ # it is a file path
      geom <- suppressWarnings(sf::st_read(geom_data, quiet = TRUE))
    } else{
      stop("Input must be existing file or sf object")
    }
  } else{
    stop("Input must be existing file or sf object")
  }

  if (large_area == FALSE){
    # fast path: drop empty geometries and test only the first feature
    geom <- geom |>
      dplyr::filter(!sf::st_is_empty(geometry))

    if (nrow(geom) == 0){
      stop("No geometry found")
    } else(
      geom <- geom[1,]
    )
  }

  # which state plane zone(s) does the geometry fall in?
  intersections <- sf::st_intersects(sf::st_make_valid(geom), state_planes_crs)

  # list each intersecting idx and
  # count how many times it was intersected
  tbl <- table(unlist(intersections))

  # get the idx that was intersected the most
  # (i.e. the zone covering the largest share of the features tested)
  sp_idx <- as.numeric(names(tbl)[which.max(tbl)])

  # use sp_idx, to get the correct crs code
  crs <- state_planes_crs[[sp_idx, "EPSG_NAD83"]]

  return(crs)
}
