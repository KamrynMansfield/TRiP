# load_tracts.R -----------------------------------------------------------
# Legacy/offline helper. The deployed app gets tract boundaries from
# get_tract_geometry() (tigris API) instead. This version reads pre-downloaded
# national tract shapefiles from a local folder. Useful for offline runs or
# batch data prep; not called anywhere in app_server().

#' Read national census tract shapefiles from disk
#'
#' Walks a directory of downloaded tract shapefiles, one folder per year, reads
#' each one, reprojects it to WGS 84 (EPSG:4326) so it lines up with the GTFS
#' route geometry, and assigns two objects per year into the global environment:
#' the full attribute layer (`tract_<year>`) and a slimmed GEOID-plus-geometry
#' version (`US_tract_<year>`).
#'
#' Expected directory layout:
#' `<base_path>/US_tract_<year>/US_tract_<year>.shp`
#'
#' @param base_path Path to the parent folder containing the yearly
#'   `US_tract_<year>` subfolders.
#' @param years The years to load. Defaults to `2017:2023`. Years with no
#'   shapefile on disk are skipped with a warning.
#'
#' @returns Nothing is returned. Called for its side effect of creating
#'   `tract_<year>` and `US_tract_<year>` objects in the global environment,
#'   and for the progress messages it prints.
#'
#' @section Side effects and caveats:
#' Assigns into `.GlobalEnv`, which is why this is not exported. Pair with
#' [tracts_by_state()] to trim the national layers down to the states you need.
#'
#' @keywords internal
#' @noRd
# load in census data
load_tracts <- function(base_path, years = 2017:2023){
  for(year in years){

    message("Loading Census Tracts for ", year, "...")

    # create path to shapefile
    shp_path <- file.path(base_path, paste0(
      "/US_tract_", year, "/US_tract_", year, ".shp"))

    # missing years are skipped rather than aborting the whole loop
    if (!file.exists(shp_path)) {
      warning("Shapefile for ", year, " not found at ", shp_path)
      next # Skip to next year
    }

    # read in tract and transform to WGS 1984
    tract <- st_read(shp_path, quiet=TRUE)

    message(year, " shapefile found and loaded")

    # reproject so tracts share a CRS with the GTFS route shapes
    tract <- tract %>%
      st_transform(4326)  # In WGS 1984 lat/long

    message(year, " shapefile transformed to WGS 1984")

    # create full version of tract information
    # (all attribute columns, named tract_<year>)
    assign(paste0("tract_", year), tract, envir = .GlobalEnv)

    # create simplified version of tract information
    # (just the join key and geometry, which is all the spatial overlay needs)
    us_tract <- tract %>%
      select(GEOID, geometry)

    assign(paste0("US_tract_", year), us_tract, envir = .GlobalEnv)

    message(" ✔ Loaded and assigned: tract_", year, " and US_tract_", year)
  }

  message("✅ All years processed.")
}
