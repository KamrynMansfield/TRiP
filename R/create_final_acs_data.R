# create_final_acs_data.R -------------------------------------------------
# Final step of the Census pipeline. Converts tract-level yearly ACS estimates
# into route-level monthly values covering the full modeling window, which is
# what make_model_data_frame() joins to the agency's UPT and VRM data.
#
# The three things happening here, in order:
#   1. Weight each tract's values by how much of the route buffer it covers,
#      then sum to the route level  -> route-level yearly values
#   2. Interpolate between years     -> route-level monthly values
#   3. Extrapolate past the last ACS year -> coverage through the end month

#' Prepare acs data for regression model
#'
#' Aggregates tract-level ACS estimates up to the route level using the buffer
#' overlap weights from [create_intersecting_tract_percentages()], then converts
#' the annual series into a continuous monthly series between `start_month` and
#' `end_month`. Annual observations are anchored to December of their year;
#' months in between are linearly interpolated, and months beyond the last ACS
#' year are linearly extrapolated using the most recent year-over-year change.
#'
#' Work-from-home (`perc_wfh`) is treated specially: rather than interpolating
#' it smoothly it is carried forward as a step function, because the sharp
#' pandemic-era shift in telecommuting is better represented as a level change
#' than as a gradual trend.
#'
#' @param combined_acs_data output from the `combine_acs_data()` function
#' @param intersecting_tracts output from the `create_intersecting_tract_percentages()` function
#' @param start_month the year and month (as a string separated by "-") that the final output will start with. Example: "2021-01"
#' @param end_month the year and month (as a string separated by "-") that the final output will end with. Example: "2025-12"
#'
#' @returns A data frame with one row per route per month between `start_month`
#'   and `end_month`, a `date` column, and one column per ACS variable. This is
#'   the `acs_data` argument expected by [create_regression_model()],
#'   [create_regression_model_forced()], and [forecast_ridership()].
#'
#' @section Warning:
#' If the tract geometry covers more years than the ACS data does, the tract
#' data is filtered down to the ACS years and a warning is emitted.
#' @export
#'
#' @examples
#' \dontrun{
#' acs_final <- create_final_acs_data(
#'   combined_acs_data   = organized_acs,
#'   intersecting_tracts = tract_buffer_data,
#'   start_month         = "2021-01",
#'   end_month           = "2025-12"
#' )
#' }
create_final_acs_data <- function(combined_acs_data, intersecting_tracts, start_month, end_month){

  # drop geometry for both inputs and make sure variables are same class
  # (geometry is no longer needed and would slow every join; the class coercion
  # guards against a character/numeric mismatch breaking the join keys)
  acs_data <- st_drop_geometry(combined_acs_data) |>
    mutate(year = as.numeric(year),
           GEOID = as.character(GEOID))
  tract_data <- st_drop_geometry(intersecting_tracts) |>
    mutate(year = as.numeric(year),
           GEOID = as.character(GEOID))

  # if the tract vintages and the ACS years don't line up, keep only the years
  # present in both and tell the user some rows were dropped
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
  # matching on both GEOID and year keeps each tract tied to its own vintage.
  # tracts with no route overlap get NA route_id and are dropped.
  tract_acs <- left_join(acs_data, tract_data, by = c("GEOID", "year")) |>
    filter(!is.na(route_id))

  # set an order to the columns so the functions will work every time
  # this matters because the aggregation below selects a column RANGE
  # (workers_16_over:perc_hshlds_noveh), so column position is load-bearing
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
      # weight every variable by the share of the route buffer this tract covers
      mutate(across(workers_16_over:perc_hshlds_noveh, ~ .x * percent_of_buffer, .names = "{.col}")) %>%
      # then sum the weighted values across all tracts touching the route,
      # producing one buffer-weighted value per route
      group_by(route_id) %>%
      summarise(across(
        workers_16_over:perc_hshlds_noveh,
        ~ sum(.x, na.rm = TRUE)
      ), .groups = "drop") |>

      # assign yearly observation to December of each year
      # (5-year ACS estimates represent the period ending that year, so anchoring
      # at year-end is the convention used for the interpolation below)
      mutate(date = ym(paste0(year_val,"-12")))

    adj_tracts[[as.character(year_val)]] <- adj_df
  }

  # combine into one data frame
  adj_data <- bind_rows(adj_tracts)

  # Pivot longer to make variable column
  # long format lets one interpolation rule be applied across every variable
  adj_long <- adj_data %>%
    pivot_longer(cols = workers_16_over:perc_hshlds_noveh, names_to = "variable", values_to = "value")

  max_date <- max(adj_long$date) # December of the most recent ACS year

  # Create monthly date sequence
  min_date <- min(adj_long$date)
  end_date <- ym(end_month)
  full_dates <- tibble(date = seq(min_date, end_date, by = "1 month")) #TODO: I'm trying to see if I need to change this


  # Expand to all combinations of route_id, variable, and date
  # But for perc_wfh, use yearly values instead of interpolating
  # (the expand_grid + left_join creates an NA for every month with no ACS
  # observation; the mutate below fills those NAs in)
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
        # (step function: telecommuting shifted abruptly, it didn't ramp)
        zoo::na.locf(value, na.rm = FALSE),
        # For all others → linear interpolation
        zoo::na.approx(value, x = date, na.rm = FALSE)
      )
    ) %>%
    ungroup()

  # Extrapolate forward to the next year using the trend from the previous years
  # For perc_wfh, the values will be linearly extrapolated despite inputting yearly values
  # NOTE: the year labels in the comments below (2023/2024) are illustrative;
  # the code is generic and always uses max_date and max_date - 1 year.

  if (end_date > max_date){
    extrapolated <- adj_filled %>%
      group_by(route_id, variable) %>%
      arrange(date) %>%
      mutate(
        value = case_when(
          # --- SPECIAL RULE FOR perc_wfh ---
          # observed and interpolated months are left alone
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
            # i.e. take the last observed year-over-year change, spread it over
            # 12 months, and project it forward month by month
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
    # nothing to project: the requested window ends inside the observed ACS range
    extrapolated <- adj_filled
  }


  # Filter to just the specified start and end dates
  # and pivot back to one column per variable for the modeling step
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
