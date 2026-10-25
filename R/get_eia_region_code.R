#' Get PADD code for specific region
#'
#' Uses the county data frame to look up the PADD code. This is used to get the proper gas
#' data from the API. See [here](https://www.eia.gov/todayinenergy/detail.php?id=4890) for
#' more details on PADD regions.
#'
#' @param counties_df The dataframe created from the function `find_overlapping_counties()`
#'
#' @returns The 3-character long PADD code for the EIA region
#' @export
#'
#' @examples
#' \dontrun{
#' padd_code <- get_eia_region_code(counties_df)
#' }
get_eia_region_code <- function(counties_df){

  # drop the geometry column (not needed for this)
  counties_df <- sf::st_drop_geometry(counties_df)

  # get the fips code as an integer
  fips <- counties_df[[1,"STATEFP"]] |>
    as.integer()

  # return the PADD code based on the corresponding state fips code
  eia_region_df$padd_code[eia_region_df$state_fips == fips]
}
