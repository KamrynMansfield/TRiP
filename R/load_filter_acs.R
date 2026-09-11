# load_filter_acs.R -------------------------------------------------------
# Legacy/offline counterpart to pull_acs_data() + combine_acs_data(). Instead
# of hitting the Census API it reads ACS tables that were previously downloaded
# into data/ACS_US/<table>/<year>/. Not called by app_server(); the live app
# uses the API path.

#' Load and filter pre-downloaded ACS CSV files
#'
#' Loops over every state / year / table combination implied by the arguments,
#' reads the matching CSV from `data/ACS_US/`, keeps only the variables listed
#' in `variable_map`, renames them to their friendly names, and tags each row
#' with its state, year, and source table.
#'
#' @param states A character vector of state identifiers used in the file names
#'   (for example `c("TN", "GA")`).
#' @param years A vector of years to load.
#' @param variable_map A data frame describing which ACS variables to keep. Must
#'   contain the columns `table` (the ACS table ID, e.g. `"B08006"`),
#'   `variable` (the full variable ID, e.g. `"B08006_017"`), and `new_name`
#'   (the readable name to rename it to, e.g. `"work_from_home"`). This is the
#'   same structure built inline by [combine_acs_data()].
#'
#' @returns A named flat list of data frames, one element per
#'   state/year/table combination, named `"<state>_<year>_<table>"`. Missing
#'   files produce a warning and are skipped.
#'
#' @section Known issue:
#' Inside the loop, `filter(table == table, ...)` compares the `variable_map`
#' column `table` to itself rather than to the loop variable, so it is always
#' `TRUE` and the table-level filter has no effect. Because the loop variable is
#' also named `table` it is masked by the column of the same name. Renaming the
#' loop variable (e.g. `tbl`) and filtering on `table == tbl` would fix it. The
#' downstream `variable %in% df$variable` filter masks most of the symptoms,
#' which is likely why this has gone unnoticed.
#'
#' @keywords internal
#' @noRd
# function to load and filter ACS data
load_filter_acs <- function(states, years, variable_map) {
  # Get unique tables from the mapping
  tables <- unique(variable_map$table)

  # Initialize flat list for results
  data_list <- list()

  # one iteration per state x year x table combination
  for (state in states) {
    for (year in years) {
      for (table in tables) {
        # files are expected at data/ACS_US/<table>/<year>/acs5_<table>_tract_<state>_<year>.csv
        file_path <- file.path(
          "data", "ACS_US", table, as.character(year),
          paste0("acs5_", table, "_tract_", state, "_", year, ".csv")
        )

        if (file.exists(file_path)) {
          df <- read.csv(file_path, stringsAsFactors = FALSE)

          # only long-format ACS extracts (one row per variable) are usable here
          if ("variable" %in% colnames(df)) {
            # Filter and rename
            # NOTE: `table == table` is a self-comparison and always TRUE (see
            # the Known issue section above); only the `variable %in%` test bites.
            filtered <- variable_map %>%
              filter(table == table, variable %in% df$variable)

            if (nrow(filtered) > 0) {
              df_filtered <- df %>%
                filter(variable %in% filtered$variable) %>%
                left_join(filtered, by = "variable") %>%
                mutate(
                  # swap the cryptic ACS ID for the readable name
                  variable = new_name,
                  state = state,
                  year = year,
                  table = table
                ) %>%
                select(-new_name)

              # key the list element so the source of each chunk stays traceable
              data_list[[paste(state, year, table, sep = "_")]] <- df_filtered
            }
          }
        } else {
          warning(paste("File not found:", file_path))
        }
      }
    }
  }

  return(data_list)
}
