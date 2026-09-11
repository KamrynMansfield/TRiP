# get_gtfs_routes.R -------------------------------------------------------
# Step 1 of the geography pipeline. Turns the agency's uploaded GTFS zip into
# one line geometry per route, which everything downstream (county lookup,
# tract buffer overlay, leaflet map) depends on.

#' Get transit route geometry
#'
#' Reads a GTFS feed and collapses it into a single line feature per route.
#' GTFS stores geometry in `shapes.txt` keyed by `shape_id`, and a route
#' typically has many shape IDs (one per pattern and direction), so this
#' function joins shapes to routes through `trips.txt` and then unions all the
#' shapes belonging to each route.
#'
#' @param gtfs_zip_file Path to the agency's GTFS zip file.
#'
#' @returns An `sf` data frame with one row per route, containing a `route_id`
#'   column and the unioned line geometry for that route, in EPSG:4326.
#'   The `route_id` values must match the `route_id` values in the uploaded
#'   ridership spreadsheet; `app_server()` checks this and blocks the user if
#'   they disagree.
#' @export
#'
#' @examples
#' \dontrun{
#' route_geom <- get_gtfs_routes("knoxville_gtfs.zip")
#' }
get_gtfs_routes <- function(gtfs_zip_file){
  # read in gtfs
  gtfs <- read_gtfs(gtfs_zip_file)

  # get route and shape id combinations
  # trips.txt is the only table linking a route to the shapes it uses
  route_list <- gtfs$trips %>%
    select(route_id, shape_id) %>%
    distinct()

  # convert gtfs to sf lines with route id
  # shapes_as_sf() turns the raw shape points into LINESTRINGs (EPSG:4326)
  shapes_routes <- shapes_as_sf(gtfs$shapes) %>%
    left_join(route_list, by = "shape_id")

  # group shapes by route id and combine shapes
  # do_union = TRUE merges the many patterns of a route into one MULTILINESTRING
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
