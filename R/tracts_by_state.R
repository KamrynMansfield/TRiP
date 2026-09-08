tracts_by_state <- function(states, lookup_path = "data/fips_code_lookup.csv", years = 2017:2023) {
  # Read in the lookup table
  lookup <- readr::read_csv(lookup_path, show_col_types = FALSE)

  # Convert state abbreviations to FIPS codes (or keep as-is if numeric input)
  if (is.character(states)) {
    fips_codes <- lookup %>%
      dplyr::filter(state %in% states) %>%
      dplyr::pull(state_code)
  } else {
    fips_codes <- stringr::str_pad(as.character(states), 2, pad = "0")
  }

  # Loop over each year and filter the tract data
  for (year in years) {
    tract <- get(paste0("tract_", year), envir = .GlobalEnv)

    filtered_tract <- tract %>%
      dplyr::filter(stringr::str_sub(GEOID, 1, 2) %in% fips_codes)

    assign(paste0("tract_", year), filtered_tract, envir = .GlobalEnv)
  }

  message("Filtered tracts for states: ", paste(states, collapse = ", "))
}
