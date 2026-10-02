#' Find most recently available year for ACS 5-year annual estimates
#'
#' @returns The most recent year the ACS 5-year estimates
#' @export
#'
#' @examples
#' get_max_acs_year()
get_max_acs_year <- function(){
  # Get all available Census API endpoints
  census_api_discovery <- fromJSON("https://api.census.gov/data.json")

  # Parse vintage and dataset names into a data frame
  api_name_vintage <- tibble(
    vintage = census_api_discovery$dataset$c_vintage,
    dataset = map_chr(census_api_discovery$dataset$c_dataset, paste, collapse = "-")
  )

  # Get the max year available
  max_year_acs_5 <- api_name_vintage |>
    filter(dataset == "acs-acs5") |>
    pull(vintage) |>
    max()

  return(max_year_acs_5)
}
