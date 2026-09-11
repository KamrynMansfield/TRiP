# combine_acs_data.R ------------------------------------------------------
# Step 5 of the pipeline: reshape the raw tidycensus output into one row per
# tract-year with one column per readable variable name, and derive the
# commute-share percentages the model actually uses.

#' Organize Pulled ACS Data
#'
#' Takes the per-year list returned by [pull_acs_data()], stacks it into a
#' single long data frame, renames the cryptic ACS variable IDs to readable
#' names, pivots to one column per variable, and converts the raw commute and
#' vehicle counts into percentages of the relevant denominator.
#'
#' @param pulled_acs_list The nested list created by `pull_acs_data()`. Should
#'   contain only successful year elements; check that the `errors` element is
#'   absent before calling this.
#'
#' @returns An `sf` data frame with one row per tract per year and one column
#'   per ACS variable, including the derived `perc_car`, `perc_taxicab`,
#'   `perc_wfh`, and `perc_hshlds_noveh` columns. The raw count columns used to
#'   build those percentages are dropped.
#' @export
#'
#' @examples
#' \dontrun{
#' acs_data_list <- pull_acs_data(county_sf, 2020:2024)
#' combined_acs  <- combine_acs_data(acs_data_list)
#' }
combine_acs_data <- function(pulled_acs_list){

  # create the variable map that lists all the variables we want and where they are found
  # (keep the three vectors aligned: table[i] / variable[i] / new_name[i] describe one variable)
  variable_map <- tibble::tibble(
    table = c("B08006", "B08006", "B08006",  "B08006", "B08006", "B08006", "B08006", "S0802", "S0802", "S0802", "S0802", "S0802", "S0802","DP05", "S2301", "S2301", "S2301", "B08201", "B08201"),
    variable = c("B08006_001", "B08006_017", "B08006_016", "B08006_002", "B08006_008", "B08006_014", "B08006_015", "S0802_C01_039", "S0802_C01_040", "S0802_C01_037", "S0802_C01_001", "S0802_C01_010", "S0802_C01_093","DP05_0001", "S2301_C02_001", "S2301_C03_001", "S2301_C04_001", "B08201_001", "B08201_002"),
    new_name = c("total_workers", "work_from_home", "taxicab", "car", "public_transit", "bike", "walk", "below_fpl", "fpl_100_150", "median_earnings", "workers_16_over", "perc_female", "perc_renter_occupied", "population", "labor_part_rate", "emp_pop_ratio", "unemp_rate", "total_hshlds", "hshlds_no_veh"))

  # Initialize flat list for results
  data_list <- list()

  # loop through each year available in the pulled_acs_list
  # and add it to the list of data frames
  # (the list name is the year, so it is copied onto the rows as a column)
  for (year_num in 1:length(pulled_acs_list)){
    year <- names(pulled_acs_list)[year_num]
    df <- pulled_acs_list[[year_num]]
    df$year <- year

    data_list[[as.character(year)]] <- df
  }

  # combined each year of data into one big dataframe
  bound_dfs <- bind_rows(data_list)

  # change the name of each variable so it is understandable
  # (e.g. "B08006_017" becomes "work_from_home")
  new_df <- bound_dfs |>
    left_join(variable_map, by = "variable") |>
    mutate(variable = new_name) |>
    select(-new_name)

  # pivot wider so there is a column for each variable.
  # margins of error and the source table name are dropped first
  final_df <- new_df %>%
    select(-moe, -table) %>%
    pivot_wider(
      names_from = variable,
      values_from = estimate
    )

  # add new commuter percent columns
  # counts are meaningless across tracts of different sizes, so convert the
  # mode-share and vehicle-access counts to percentages of their denominators
  final_df_edit <- final_df %>%
    mutate(perc_car = round((car/total_workers)*100,1),
           perc_taxicab = round((taxicab/total_workers)*100,1),
           perc_wfh = round((work_from_home/total_workers)*100,1),
           perc_hshlds_noveh = round((hshlds_no_veh/total_hshlds)*100,1))

  # remove old commute columns
  # (raw counts and their denominators are no longer needed once the
  # percentages exist; public_transit, bike and walk are pulled but unused)
  final_df_edit <- final_df_edit %>%
    select(-c(total_workers, car, public_transit, bike, walk, taxicab, work_from_home, total_hshlds, hshlds_no_veh))

  return(final_df_edit)

}

# combined_acs <- combine_acs_data(acs_data_list)
