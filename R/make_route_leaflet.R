make_route_leaflet <- function(routes_sf, county_sf){
  pal <- colorFactor(viridis(50), domain = routes_sf$route_id)

  leaflet(routes_sf) |>
    addProviderTiles("CartoDB.Positron") |>

    addPolygons(data = county_sf, color = "grey", weight = 2) |>

    # Draw routes
    addPolylines(
      color = ~pal(route_id),
      weight = 2,
      opacity = 0.7,
      highlightOptions = highlightOptions(
        weight = 4,
        color = "red",
        bringToFront = TRUE
      ),
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
