# load in census data
load_tracts <- function(base_path, years = 2017:2023){
  for(year in years){

    message("Loading Census Tracts for ", year, "...")

    # create path to shapefile
    shp_path <- file.path(base_path, paste0(
      "/US_tract_", year, "/US_tract_", year, ".shp"))

    if (!file.exists(shp_path)) {
      warning("Shapefile for ", year, " not found at ", shp_path)
      next # Skip to next year
    }

    # read in tract and transform to WGS 1984
    tract <- st_read(shp_path, quiet=TRUE)

    message(year, " shapefile found and loaded")

    tract <- tract %>%
      st_transform(4326)  # In WGS 1984 lat/long

    message(year, " shapefile transformed to WGS 1984")

    # create full version of tract information
    assign(paste0("tract_", year), tract, envir = .GlobalEnv)

    # create simplified version of tract information
    us_tract <- tract %>%
      select(GEOID, geometry)

    assign(paste0("US_tract_", year), us_tract, envir = .GlobalEnv)

    message(" ✔ Loaded and assigned: tract_", year, " and US_tract_", year)
  }

  message("✅ All years processed.")
}
