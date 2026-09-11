# pull_acs_data.R ---------------------------------------------------------
# Step 4 of the pipeline: download the raw ACS variables for every tract in the
# route counties, one pull per year. Output feeds combine_acs_data().

#' Use the county data frame to get needed acs tables
#'
#' Downloads the fixed set of ACS 5-year variables the TRiP model relies on, at
#' the census tract level, for every county the routes pass through. One API
#' call is made per year so that each year can be matched to the tract vintage
#' of the same year.
#'
#' Requires a Census API key to be registered (the app collects one on tab 3 and
#' installs it with `tidycensus::census_api_key()` before calling this).
#'
#' The variables pulled come from five tables:
#' \describe{
#'   \item{B08006}{Means of transportation to work (total workers, WFH, taxi, car, transit, bike, walk)}
#'   \item{B08201}{Household size by vehicles available (total households, households with no vehicle)}
#'   \item{DP05}{Demographic profile (total population)}
#'   \item{S0802}{Commuting characteristics (poverty ratios, median earnings, share female, share renter-occupied)}
#'   \item{S2301}{Employment status (labor force participation, employment/population ratio, unemployment)}
#' }
#'
#' @param county_sf the county sf object that was output by `find_overlapping_counties()`
#' @param years the years that you want the tables for. Defaults to 2024.
#'
#' @returns A named list with one element per year holding a long-format
#'   `tidycensus` data frame (with geometry), plus a final `errors` element
#'   listing any years that failed to download. `app_server()` checks for a
#'   non-empty `errors` element and warns the user rather than proceeding.
#' @export
#'
#' @examples
#' \dontrun{
#' acs_data_list <- pull_acs_data(county_sf, 2020:2024)
#' }
pull_acs_data <- function(county_sf, years = 2024){
  # every state and county the routes touch
  states <- unique(county_sf$STATEFP)
  counties <- unique(county_sf$COUNTYFP)

  geography_name <- "tract" # census tract is geography for 5-year estimates
  acs_estimate <- "acs5"    # 5-year estimates, do acs1 for 1-year estimates
  # table_names <- c("B08006","B08201", "DP05","S0802","S2301") # these are the tables we want

  # the exact variable IDs requested; combine_acs_data() maps these to readable names
  vars <- c("B08006_001", "B08006_017", "B08006_016", "B08006_002", "B08006_008", "B08006_014", "B08006_015", "S0802_C01_039", "S0802_C01_040", "S0802_C01_037", "S0802_C01_001", "S0802_C01_010", "S0802_C01_093","DP05_0001", "S2301_C02_001", "S2301_C03_001", "S2301_C04_001", "B08201_001", "B08201_002")

  errors <- c() # start empty vector to store any errors
  year_list <- list() # start empty list to store acs tables
  # loop through each year to get a table for each year
  for (year in years){

    # pull that data (return NA if it didn't get pulled)
    # geometry = TRUE so the tract polygons come back attached
    data <- tryCatch({

      get_acs(
        geography = geography_name,
        variables = vars,
        year = year,
        survey = acs_estimate,
        state = states,
        county = counties,
        geometry = TRUE
      )

    }, error = function(e) {
      return(NULL)
    })

    # record which year failed so the caller can surface a useful message
    if (is.null(data)){
      errors <- c(errors, year)
    }
    # add the table to the list for the year we just ran
    year_list[[as.character(year)]] <- data
  }

  # add the errors to the end of the list
  # Worth knowing: if `errors` is NULL (the happy path) this assignment removes
  # rather than creates the element, so "errors" %in% names(year_list) is FALSE
  # when every year downloaded cleanly. That is exactly what app_server() relies
  # on. On the failure path the element does appear, and combine_acs_data()
  # would try to bind that character vector as if it were a data frame, so
  # callers must check for errors before passing the list along.
    year_list[["errors"]] <- errors

  return(year_list)
}

# acs_data_list <- pull_acs_data(county_sf, 2020:2024)
