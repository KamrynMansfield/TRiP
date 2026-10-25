# tracts_by_state.R -------------------------------------------------------
# Companion to load_tracts.R. Both belong to the older, offline workflow in
# which national tract shapefiles were read from disk and stored in the global
# environment. The live app no longer calls either one; it pulls tracts on
# demand through get_tract_geometry() (tigris). Kept for reference and for
# local/offline data prep.

#' Filter globally loaded tract layers down to selected states
#'
#' Assumes [load_tracts()] has already been run, so that objects named
#' `tract_2017`, `tract_2018`, ... exist in the global environment. For each
#' requested year this function keeps only the tracts whose GEOID begins with
#' one of the target state FIPS codes, then writes the filtered layer back over
#' the original global object.
#'
#' @param states Either a character vector of two-letter state abbreviations
#'   (for example `c("TN", "GA")`), which are looked up in the FIPS lookup
#'   table, or numeric state FIPS codes, which are zero-padded to two digits.
#' @param lookup_path Path to the CSV lookup table mapping state abbreviations
#'   to FIPS codes. Must contain `state` and `state_code` columns.
#' @param years The years of tract objects to filter. Defaults to `2017:2023`.
#'
#' @returns Nothing is returned. Called for its side effect of overwriting the
#'   `tract_<year>` objects in the global environment, and for the status
#'   message it prints.
#'
#' @section Side effects and caveats:
#' This function both reads from and assigns into `.GlobalEnv`, which is why it
#' is not exported. It also depends on `stringr`, which is not currently listed
#' in the package DESCRIPTION's Imports field.
#'
#' @keywords internal
#' @noRd
tracts_by_state <- function(states, lookup_path = "data/fips_code_lookup.csv", years = 2017:2023) {
  # Read in the lookup table
  lookup <- readr::read_csv(lookup_path, show_col_types = FALSE)

  # Convert state abbreviations to FIPS codes (or keep as-is if numeric input)
  if (is.character(states)) {
    fips_codes <- lookup %>%
      dplyr::filter(state %in% states) %>%
      dplyr::pull(state_code)
  } else {
    # numeric input: pad to the two-character form used in GEOIDs (e.g. 6 -> "06")
    fips_codes <- stringr::str_pad(as.character(states), 2, pad = "0")
  }

  # Loop over each year and filter the tract data
  for (year in years) {
    # grab the tract layer that load_tracts() put in the global environment
    tract <- get(paste0("tract_", year), envir = .GlobalEnv)

    # the first two characters of a tract GEOID are the state FIPS code
    filtered_tract <- tract %>%
      dplyr::filter(stringr::str_sub(GEOID, 1, 2) %in% fips_codes)

    # overwrite the global object with the filtered version
    assign(paste0("tract_", year), filtered_tract, envir = .GlobalEnv)
  }

  message("Filtered tracts for states: ", paste(states, collapse = ", "))
}
