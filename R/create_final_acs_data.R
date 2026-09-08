#' Prepare acs data for regression model
#'
#' @param combined_acs_data output from the `combine_acs_data()` function
#' @param intersecting_tracts output from the `create_intersecting_tract_percentages()` function
#' @param start_month the year and month (as a string separated by "-") that the final output will start with. Example: "2021-01"
#' @param end_month the year and month (as a string separated by "-") that the final output will end with. Example: "2025-12"
#'
#' @returns a data frame with the needed variables to go into the regression model
#' @export
#'
#' @examples
create_final_acs_data <- function(combined_acs_data, intersecting_tracts, start_month, end_month){

  # drop geometry for both inputs and make sure variables are same class
  acs_data <- st_drop_geometry(combined_acs_data) |>
    mutate(year = as.numeric(year),
           GEOID = as.character(GEOID))
  tract_data <- st_drop_geometry(intersecting_tracts) |>
    mutate(year = as.numeric(year),
           GEOID = as.character(GEOID))

  if (sum(unique(acs_data$year) %in% (unique(tract_data$year))) != length(unique(acs_data$year))){
    tract_data <- tract_data |>
      filter(year %in% acs_data$year)
    warning(paste0("The intersecting tracts data is from ",
                  min(intersecting_tracts$year), "-",max(intersecting_tracts$year),
                  " but the acs data is only from ",
                  min(acs_data$year), "-",max(acs_data$year),
                  ". Intersecting tracts data will therefore be filtered to the acs years and drop some rows.")
            )
  }

  # join the two
  tract_acs <- left_join(acs_data, tract_data, by = c("GEOID", "year")) |>
    filter(!is.na(route_id))

  # set an order to the columns so the functions will work every time
  col_order <- c("GEOID","route_id","intersect_area","buffer_area","percent_of_buffer","NAME",
  "year","workers_16_over","perc_female","median_earnings","below_fpl","fpl_100_150",
  "perc_renter_occupied","population","labor_part_rate","emp_pop_ratio","unemp_rate","perc_car",
  "perc_taxicab","perc_wfh","perc_hshlds_noveh")

  # reordering the columns
  tract_acs <- tract_acs |>
    select(any_of(col_order))

  # loop through each year to calculate adjusted acs estimates
  adj_tracts <- list()
  for (year_val in unique(tract_acs$year)){

    # get data from just the specified year
    tract_df <- tract_acs |>
      filter(year == year_val)

    # get adjusted estimates for tracts, then sum for each route
    adj_df <- tract_df %>%
      mutate(across(workers_16_over:perc_hshlds_noveh, ~ .x * percent_of_buffer, .names = "{.col}")) %>%
      group_by(route_id) %>%
      summarise(across(
        workers_16_over:perc_hshlds_noveh,
        ~ sum(.x, na.rm = TRUE)
      ), .groups = "drop") |>

      # assign yearly observation to December of each year
      mutate(date = ym(paste0(year_val,"-12")))

    adj_tracts[[as.character(year_val)]] <- adj_df
  }

  # combine into one data frame
  adj_data <- bind_rows(adj_tracts)

  # Pivot longer to make variable column
  adj_long <- adj_data %>%
    pivot_longer(cols = workers_16_over:perc_hshlds_noveh, names_to = "variable", values_to = "value")

  max_date <- max(adj_long$date)

  # Create monthly date sequence
  min_date <- min(adj_long$date)
  end_date <- ym(end_month)
  full_dates <- tibble(date = seq(min_date, end_date, by = "1 month")) #TODO: I'm trying to see if I need to change this


  # Expand to all combinations of route_id, variable, and date
  # But for perc_wfh, use yearly values instead of interpolating
  adj_filled <- expand_grid(
    route_id = unique(adj_long$route_id),
    variable = unique(adj_long$variable),
    date = full_dates$date
  ) %>%
    left_join(adj_long, by = c("route_id", "variable", "date")) %>%
    group_by(route_id, variable) %>%
    arrange(date) %>%
    mutate(
      value = if_else(
        variable == "perc_wfh",
        # For perc_wfh → carry last yearly value forward
        zoo::na.locf(value, na.rm = FALSE),
        # For all others → linear interpolation
        zoo::na.approx(value, x = date, na.rm = FALSE)
      )
    ) %>%
    ungroup()

  # Extrapolate forward to the next year using the trend from the previous years
  # For perc_wfh, the values will be linearly extrapolated despite inputting yearly values

  if (end_date > max_date){
    extrapolated <- adj_filled %>%
      group_by(route_id, variable) %>%
      arrange(date) %>%
      mutate(
        value = case_when(
          # --- SPECIAL RULE FOR perc_wfh ---
          variable == "perc_wfh" & date <= max_date ~ value,

          # Hold max year value constant until 2024-11-01
          variable == "perc_wfh" & date > max_date & date <= end_date ~
            value[date == max_date],

          # From 2024-12-01 onward, add the full annual increment (2022→2023)
          variable == "perc_wfh" & date >= end_date + months(1) ~ {
            val_2023 <- value[date == max_date - years(1)]
            val_2024 <- value[date == max_date]
            annual_increment <- val_2024 - val_2023
            val_2024 + annual_increment
          },

          # --- DEFAULT RULE FOR ALL OTHER VARIABLES ---
          date <= max_date ~ value,  # keep known & interpolated values
          TRUE ~ {
            # Linear monthly extrapolation using the change from 2022-12-01 → 2023-12-01
            val_2023 <- value[date == max_date - years(1)]
            val_2024 <- value[date == max_date]

            increment <- (val_2024 - val_2023) / 12
            months_ahead <- interval(max_date, date) %/% months(1)
            val_2024 + increment * months_ahead
          }
        )
      ) %>%
      ungroup()
  } else{
    extrapolated <- adj_filled
  }


  # Filter to just the specified start and end dates
  start_date <- ym(start_month)
  adj_monthly <- extrapolated %>%
    filter(date >= start_date, date <= end_date) %>%
    pivot_wider(names_from = variable, values_from = value)

  return(adj_monthly)

}

# combined_acs_data <- organized_acs
# intersecting_tracts <- tract_buffer_data
# end_month <- month_end
# start_month <- month_start

# acs_final_test <- create_final_acs_data(combined_acs_data,
#                                         intersecting_tracts,
#                                         start_month,
#                                         end_month)
