#' Get transit route geometry
#'
#' @param gtfs_zip_file The gtfs zip file
#'
#' @returns The route geometry from the file
#' @export
#'
#' @examples
get_gtfs_routes <- function(gtfs_zip_file){
  # read in gtfs
  gtfs <- read_gtfs(gtfs_zip_file)

  # get route and shape id combinations
  route_list <- gtfs$trips %>%
    select(route_id, shape_id) %>%
    distinct()

  # convert gtfs to sf lines with route id
  shapes_routes <- shapes_as_sf(gtfs$shapes) %>%
    left_join(route_list, by = "shape_id")

  # group shapes by route id and combine shapes
  comb_shapes_routes <- shapes_routes %>%
    group_by(route_id) %>%
    summarise(do_union = TRUE)

  return(comb_shapes_routes)
}

# route_geom <- get_gtfs_routes("../data/Nashville/GTFS/2024-12-16.zip")

# gtfs_zip_file <- "../data/MARTA Data/marta_gtfs_12-14-2021.zip"
#
# route_geom <- comb_shapes_routes |>
#   filter(route_id %in% bus_routes)
