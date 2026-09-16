# make_model_data_frame.R -------------------------------------------------
# The single place where all three data sources come together: the agency's
# uploaded ridership spreadsheet, the route-level monthly ACS data, and the
# regional gas price series. Called by create_regression_model(),
# create_regression_model_forced(), and forecast_ridership(), so any change here
# affects estimation and forecasting alike.

#' Build the modeling data frame
#'
#' Joins agency ridership data to route-level ACS data and gas prices, converts
#' monthly UPT into an average-weekday figure, adds the time-trend and
#' seasonality terms, applies an adult base fare series, and log-transforms
#' every continuous variable so the estimated coefficients can be read as
#' elasticities.
#'
#' @param data_xlsx The agency's uploaded ridership data as a data frame. Must
#'   contain `route_id`, `month`, `year`, `upt`, and `vrm`; may optionally
#'   contain `brt` and any number of extra numeric columns, which are picked up
#'   automatically and log-transformed.
#' @param acs_data Route-level monthly ACS data, as returned by
#'   [create_final_acs_data()]. Must contain `route_id`, `date`, and the ACS
#'   variable columns.
#' @param gas_data Monthly gas price data with `month`, `year`, and `gas_price`
#'   columns. The app passes the internal `gas` object, sourced from the
#'   [U.S. Energy Information Administration](https://www.eia.gov/dnav/pet/pet_pri_gnd_dcus_nus_m.htm).
#' @param fare_df Optional data frame describing adult base fare changes, with
#'   `change_date`, `prev_fare`, and `new_fare` columns (built by the fare table
#'   on tab 2). If `NULL`, fare is held at a constant 1 so that `log_fare` is 0
#'   and the variable drops out of the model.
#'
#' @returns A data frame with one row per route-month, containing the log-
#'   transformed model variables (`log_upt_avg`, `log_vrm`, `log_gas_price`,
#'   the `log_perc_*` ACS variables, `log_fare`), the untransformed helpers
#'   (`year`, `month` as a factor, `year_cent`, `weekdays_in_month`, `time`),
#'   and a `brt` logical column.
#'
#' @section Variables deliberately left untransformed:
#' `workers_16_over`, `median_earnings`, and `emp_pop_ratio` are not logged.
#' `year`, `year_cent`, `month`, and `brt` are also left as-is because they
#' enter the model as a trend, a factor, and an indicator respectively.
#'
#' @keywords internal
#' @noRd
make_model_data_frame <- function(data_xlsx,
                                    acs_data,
                                    gas_data,
                                  fare_df = NULL){
  # load in upt and vrm data
  data <- data_xlsx

  min_year <- min(data$year) # baseline year for the centered time trend

  # create a data frame with a time column showing the months from the first month in dataset
  # (a sequential counter, 1 = first month observed; not currently used in the
  # model formula but kept for inspection and plotting)
  time_key <- data |>
    group_by(year, month) |>
    summarise(.groups = "drop_last") |>
    ungroup() |>
    arrange(month) |>
    arrange(year) # sorting by month first then year yields chronological order

  time_key$time <- 1:nrow(time_key)

  df <- data |>
    left_join(time_key, by = c("year","month"))

  # create column with # of weekdays per month
  # needed to convert monthly UPT totals into an average weekday figure
  df <- count_weekdays(df)

  # create centered year to later estimate time trend
  # centering at the first year of data keeps year_cent^2 well scaled
  df$year_cent <- df$year - min_year

  # load in preprocessed monthly American Community Survey data
  acs <- acs_data

  # load in gas_prices data
  gas <- gas_data

  # create month and year column for ACS
  # (ACS arrives with a date column; split it so it joins on the same keys)
  acs <- acs %>%
    mutate(month = month(date),
           year = year(date)) %>%
    select(-date)

  # force the join key to a common type; route IDs are often numeric-looking
  # strings and a type mismatch would silently drop every row
  df$route_id <- as.character(df$route_id)
  acs$route_id <- as.character(acs$route_id)

  # combine gas prices and acs with upt and vrm
  # gas varies by month only; ACS varies by month and route
  df_all <- df %>%
    left_join(gas, by = c("month", "year")) %>%
    left_join(acs, by = c("month", "year", "route_id")) %>%
    mutate(month = factor(month)) # factor so it enters the model as seasonality dummies

  # make upt a weekday average using number of weekdays in the month
  # this removes the sawtooth caused by months having different weekday counts
  df_all <- df_all %>%
    mutate(upt_avg = upt/weekdays_in_month)

  #' list variables to be log transformed
  #' three variables not log transformed: "workers_16_over", "median_earnings", "emp_pop_ratio"
  vars_to_log <- c("upt", "upt_avg", "vrm", "gas_price", "perc_car", "perc_taxicab", "perc_wfh", "perc_female", "below_fpl", "fpl_100_150", "perc_renter_occupied", "population", "labor_part_rate", "unemp_rate", "perc_hshlds_noveh")

  # add any variables from the vrm_data
  # anything the agency added beyond the five required columns is treated as a
  # candidate predictor and logged too (this is why the app rejects uploads with
  # "log_" in a column name and requires extra columns to be numeric)
  extra_vars <- names(data)[!names(data) %in% c("route_id","month","year","upt","vrm")]
  vars_to_log <- c(vars_to_log, extra_vars)

  # log transform vars_to_log variables
  # logging both sides makes each coefficient an elasticity, which is what the
  # forecasting step assumes when it applies percent changes
  df_all_log <- df_all %>%
    mutate(across(
      all_of(vars_to_log),
      ~ if_else(. > 0, log(.), NA_real_),   # safely take log only if positive
      .names = "log_{.col}"          # rename: log_variable
    )) %>%
    select(-all_of(vars_to_log))

  # add fares column

  if (!is.null(fare_df)){
    # start everyone at the fare in effect before the earliest recorded change
    oiriginal_fare <- fare_df |>
      filter(change_date == min(change_date)) |>
      select(prev_fare) |>
      unlist()

    df_all_log$fare <- oiriginal_fare
    df_all_log$month_numeric <- as.numeric(df_all_log$month)

    # walk the fare changes in order, overwriting the fare for every month at or
    # after each change date
    for (row_id in 1:nrow(fare_df)){
      date_used <- ymd(fare_df$change_date[[row_id]])
      new_fare <- as.numeric(fare_df$new_fare[[row_id]])

      # update the fare with the new fare on the dates after the fare change
      df_all_log <- df_all_log |>
        dplyr::mutate(fare = ifelse(lubridate::ym(paste(year, month_numeric)) >= date_used, new_fare, fare))
    }
    df_all_log$month_numeric <- NULL
  } else{
    # no fare history: constant 1 means log_fare is 0 for every row, so the
    # variable carries no information and is effectively neutral
    df_all_log$fare <- 1
  }

  df_all_log$log_fare <- if_else(df_all_log$fare > 0, log(df_all_log$fare), NA_real_)

  df_all_log$fare <- NULL # only the logged version is used in the model

  # if the agency didn't supply a brt column, assume no route is BRT
  if (is.null(df_all_log$brt)){
    df_all_log$brt <- FALSE
  }

  return(df_all_log)
}
