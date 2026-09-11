# get_tract_geometry.R ----------------------------------------------------
# Step 3 of the geography pipeline: pull census tract boundaries for the
# counties the routes touch, one layer per year. These are overlaid with the
# route buffer in create_intersecting_tract_percentages().

#' Get census tract geometry for given counties
#'
#' This function uses the tigris package to retrieve the census tract boundaries
#' for every requested year and stacks them into a single `sf` object with a
#' `year` column. Because tract boundaries are redrawn between vintages, each
#' year must be fetched separately and later joined to ACS data of the same year.
#'
#' Any failure (no internet, a Census.gov outage, an unreleased vintage, or an
#' empty result) causes the function to return `NULL` rather than partial data.
#' `app_server()` checks for that `NULL` and shows the user a modal explaining
#' the likely causes.
#'
#' @param state_fips A state fips or vector of state fips
#' @param county_fips A county fips or vector of county fips
#' @param years A year or list of years
#'
#' @returns An sf object of the requested tract boundaries that have been
#'   validated using `st_make_valid`, with an added `year` column. Returns
#'   `NULL` if any requested year failed to download or came back empty.
#' @export
#'
#' @examples
#' \dontrun{
#' state_fips  <- unique(county_sf$STATEFP)
#' county_fips <- unique(county_sf$COUNTYFP)
#' tract_geom  <- get_tract_geometry(state_fips, county_fips, 2020:2024)
#' }
get_tract_geometry <- function(state_fips, county_fips, years){

  errors <- c()      # flags any year that failed or returned nothing
  tracts_list <- list() # one sf object per year, combined at the end

  for (year in years){

    # tigris downloads can fail for lots of reasons (offline, shutdown, vintage
    # not published yet), so wrap it and record NULL instead of erroring out
    tracts_sf <- tryCatch({
      tigris::tracts(state = state_fips, county = county_fips, year = year) |>
        mutate(year = year) # tag the vintage so it can be joined to matching ACS data
    }, error = function(e) {
      return(NULL)
    })

    # treat both a failed download and an empty result as an error
    if (is.null(tracts_sf)){
      errors <- c(errors, 1)
    } else if (nrow(tracts_sf) == 0){
      errors <- c(errors, 1)
    }

    tracts_list[[as.character(year)]] <- tracts_sf
  }

  combined_tracts <- bind_rows(tracts_list)

  # all-or-nothing: only return data if every year came back clean
  if (is.null(errors)){
    # st_make_valid() repairs self-intersecting rings so st_intersection() later won't fail
    validated_tracts <- st_make_valid(combined_tracts)
    return(validated_tracts)
  } else{
    return(NULL)
  }
}

# state_fips <- unique(county_sf$STATEFP)
# county_fips <- unique(county_sf$COUNTYFP)
# tract_geom <- get_tract_geometry(state_fips, county_fips, 2020:2024)
