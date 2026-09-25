# make_route_leaflet.R ----------------------------------------------------
# Builds the interactive map shown on tab 3 (GTFS Upload) so the user can
# confirm the uploaded feed and the counties it touches look right before any
# Census data is pulled.

#' Build the interactive route map
#'
#' Draws the agency's bus routes over a light basemap, with the overlapping
#' county boundaries underneath for context. Each route gets its own color, and
#' hovering over a route highlights it in red and shows its `route_id` as a
#' tooltip so the user can confirm the IDs match their ridership file.
#'
#' @param routes_sf An `sf` object of route geometries with a `route_id`
#'   column, as returned by [get_gtfs_routes()].
#' @param county_sf An `sf` object of county polygons, as returned by
#'   [find_overlapping_counties()], drawn as a grey reference layer.
#'
#' @returns A `leaflet` htmlwidget, rendered by `renderLeaflet()` in
#'   `app_server()`.
#' @export
#'
#' @examples
#' \dontrun{
#' routes_sf <- get_gtfs_routes("gtfs.zip")
#' make_route_leaflet(routes_sf, find_overlapping_counties(routes_sf))
#' }
make_route_leaflet <- function(routes_sf, county_sf){
  # one distinct color per route, sampled from the viridis palette
  pal <- colorFactor(viridis(50), domain = routes_sf$route_id)

  # TODO: make sure the key is working when I publish it.

  # routes_sf <- get_gtfs_routes("../test_files/nashville_gtfs.zip")
  # county_sf <- find_overlapping_counties(routes_sf)

  leaflet(routes_sf) |>

    # muted basemap so the route colors stay readable
    addTiles(
      urlTemplate = paste0("https://basemaps.cartocdn.com/rastertiles/light_all/{z}/{x}/{y}{r}.png?key=", my_carto_key),
      attribution = '&copy; <a href="https://carto.com/attributions">CARTO</a> &copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>'
    ) |>

    # county outlines drawn first so they sit beneath the routes
    addPolygons(data = county_sf, color = "grey", weight = 2) |>

    # Draw routes
    addPolylines(
      color = ~pal(route_id),
      weight = 2,
      opacity = 0.7,
      # thicken and recolor the route the cursor is over
      highlightOptions = highlightOptions(
        weight = 4,
        color = "red",
        bringToFront = TRUE
      ),
      # tooltip text = the route ID, which is what the user needs to verify
      label = ~route_id,
      # Configure label options for hover behavior
      labelOptions = labelOptions(
        noHide = FALSE, # Label hides when mouse moves off (default, but good to be explicit)
        direction = "top", # Position the label
        textsize = "15px" # Customize appearance
      )
    )
}
#
# routes_sf <- get_gtfs_routes("../../data/test_agency/knoxville_gtfs.zip")
# routes_sf <- get_gtfs_routes("../../data/Nashville/GTFS/2024-12-16.zip")
#
