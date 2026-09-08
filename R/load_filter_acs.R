# function to load and filter ACS data
load_filter_acs <- function(states, years, variable_map) {
  # Get unique tables from the mapping
  tables <- unique(variable_map$table)

  # Initialize flat list for results
  data_list <- list()

  for (state in states) {
    for (year in years) {
      for (table in tables) {
        file_path <- file.path(
          "data", "ACS_US", table, as.character(year),
          paste0("acs5_", table, "_tract_", state, "_", year, ".csv")
        )

        if (file.exists(file_path)) {
          df <- read.csv(file_path, stringsAsFactors = FALSE)

          if ("variable" %in% colnames(df)) {
            # Filter and rename
            filtered <- variable_map %>%
              filter(table == table, variable %in% df$variable)

            if (nrow(filtered) > 0) {
              df_filtered <- df %>%
                filter(variable %in% filtered$variable) %>%
                left_join(filtered, by = "variable") %>%
                mutate(
                  variable = new_name,
                  state = state,
                  year = year,
                  table = table
                ) %>%
                select(-new_name)

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
