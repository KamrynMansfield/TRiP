#' Use to county data frame to get needed acs tables
#'
#' @param county_sf the county sf object that was output by `find_overlapping_counties()`
#' @param years the years that you want the tables for
#'
#' @returns a nested list with acs data for each year and each table we want (tables: "B08006","B08201", "DP05","S0802","S2301")
#' @export
#'
#' @examples
pull_acs_data <- function(county_sf, years = 2024){
  states <- unique(county_sf$STATEFP)
  counties <- unique(county_sf$COUNTYFP)

  geography_name <- "tract" # census tract is geography for 5-year estimates
  acs_estimate <- "acs5"    # 5-year estimates, do acs1 for 1-year estimates
  # table_names <- c("B08006","B08201", "DP05","S0802","S2301") # these are the tables we want
  vars <- c("B08006_001", "B08006_017", "B08006_016", "B08006_002", "B08006_008", "B08006_014", "B08006_015", "S0802_C01_039", "S0802_C01_040", "S0802_C01_037", "S0802_C01_001", "S0802_C01_010", "S0802_C01_093","DP05_0001", "S2301_C02_001", "S2301_C03_001", "S2301_C04_001", "B08201_001", "B08201_002")

  errors <- c() # start empty vector to store any errors
  year_list <- list() # start empty list to store acs tables
  # loop through each year to get a table for each year
  for (year in years){

    # pull that data (return NA if it didn't get pulled)
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

    if (is.null(data)){
      errors <- c(errors, year)
    }
    # add the table to the list for the year we just ran
    year_list[[as.character(year)]] <- data
  }

  # add the errors to the end of the list
    year_list[["errors"]] <- errors

  return(year_list)
}

# acs_data_list <- pull_acs_data(county_sf, 2020:2024)

