#' Get census tract geometry for given counties
#'
#' This function uses the tirgis package to retrieve the census tract boundaries.
#'
#' @param state_fips A state fips or vector of state fips
#' @param county_fips A county fips or vector of county fips
#' @param years A year or list of years
#'
#' @returns An sf object of the requested tract boundaries that have been validated using `st_make_valid`
#' @export
#'
#' @examples
get_tract_geometry <- function(state_fips, county_fips, years){

  errors <- c()
  tracts_list <- list()
  for (year in years){

    tracts_sf <- tryCatch({
      tigris::tracts(state = state_fips, county = county_fips, year = year) |>
        mutate(year = year)
    }, error = function(e) {
      return(NULL)
    })

    if (is.null(tracts_sf)){
      errors <- c(errors, 1)
    } else if (nrow(tracts_sf) == 0){
      errors <- c(errors, 1)
    }

    tracts_list[[as.character(year)]] <- tracts_sf
  }

  combined_tracts <- bind_rows(tracts_list)

  if (is.null(errors)){
    validated_tracts <- st_make_valid(combined_tracts)
    return(validated_tracts)
  } else{
    return(NULL)
  }
}

# state_fips <- unique(county_sf$STATEFP)
# county_fips <- unique(county_sf$COUNTYFP)
# tract_geom <- get_tract_geometry(state_fips, county_fips, 2020:2024)
