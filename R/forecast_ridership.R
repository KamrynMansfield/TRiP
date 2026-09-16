# forecast_ridership.R ----------------------------------------------------
# The forecasting engine. Rather than calling predict() on the fitted model, it
# applies the model's elasticities multiplicatively to a reference month's
# observed ridership.
#
# The logic in one line:
#   forecast UPT = reference UPT x (elasticity factors) x (year trend factor)
#                  x (seasonality factor) x (BRT factor)
#
# Because the model is log-log with route fixed effects, each coefficient is an
# elasticity, so a proportional change in a driver translates into a
# proportional change in ridership via (1 + change)^elasticity. Anchoring on an
# observed month means the route fixed effects cancel out and never need to be
# extracted.

# coefs <- final_coefs
# data_xlsx <- vrm_data
# acs_data <- acs
# gas_data <- "data/Midwest_All_Grades_All_Formulations_Retail_Gasoline_Prices.csv"
# scenario_inputs_df <- scenario_df
# start_year <- NULL
# start_month <- NULL

# scenario_inputs_df_with_routes <- expand.grid("Route" = routes,
#   "Variable" = names(elasticities),
#                           "Low.Estimate" = "-1",
#                           "Mid.Estimate" = "2%",
#                           "High.Estimate" = "5%")

# scenario_inputs_df <- expand.grid("Variable" = names(elasticities),
#                           "Low.Estimate" = "-1%",
#                           "Mid.Estimate" = "2%",
#                           "High.Estimate" = "5%")
#
# new_scenario_inputs_df <- data.frame(variable = scenario_inputs_df$Variable,
#                                      Low = scenario_inputs_df$Low.Estimate,
#                                      Mid = scenario_inputs_df$Mid.Estimate,
#                                      High = scenario_inputs_df$High.Estimate)


# In the data, there are a few potential variables that won't be logged
# year, year^2, month, brt
# all the rest are logged

#' Forecast route-level ridership under Low, Medium, and High scenarios
#'
#' Projects average weekday ridership forward through December of the year
#' following the reference month, for every route and all three scenarios. The
#' forecast is built by growing an observed reference month by a product of
#' factors: one per elasticity variable, one for the linear time trend, one for
#' the quadratic trend, one for monthly seasonality, and one for any BRT
#' conversion.
#'
#' @param coefs A named numeric vector of model coefficients, typically the
#'   output of the `final_coefs()` reactive (model coefficients with any
#'   user-forced literature values substituted in).
#' @param data_xlsx The agency's uploaded ridership data as a data frame.
#' @param acs_data Route-level monthly ACS data from [create_final_acs_data()].
#' @param gas_data Internal data storing historical gas prices obtained
#' from [U.S. Energy Information Administration](https://www.eia.gov/dnav/pet/pet_pri_gnd_dcus_nus_m.htm)
#' @param scenario_inputs_df The saved scenario table, with `Variable`,
#'   `Low.Estimate`, `Mid.Estimate`, and `High.Estimate` columns. Passed to
#'   [organize_scenario_df()].
#' @param start_year Year to anchor the forecast to. If `NULL` (the default) the
#'   most recent year in the data is used.
#' @param start_month Month to anchor the forecast to. If `NULL` (the default)
#'   the last observed month of `start_year` is used.
#' @param fare_df Optional data frame of adult base fare changes.
#' @param brt_df Optional data frame of planned BRT conversions, with
#'   `change_date_brt` and `routes_brt` columns, as built by the BRT table on
#'   tab 5.
#' @param five_year Option ability to forecast for the next 5 years instead of
#' the next year. When `FALSE` (the default), it will only run a 1-year forecast.
#'
#' @returns A data frame combining observed history and forecast, with columns
#'   `route_id`, `year`, `month`, `avg_daily_upt`, `tot_weekday_upt`,
#'   `forecast` (logical), `scenario`, and `date`. Includes a synthetic
#'   `"all_routes"` route that sums every route, and duplicates the reference
#'   month across all three scenarios so the forecast lines connect cleanly to
#'   the observed line when plotted.
#'
#' @section Argument mismatches worth checking:
#' Same two issues flagged in [create_regression_model_forced()]:
#' `app_server()` calls this with `gas_data = gas` while the formal here is
#' `gas_data` (an "unused argument" error), and the call to
#' `make_model_data_frame()` below passes five arguments to a four-argument
#' function by adding `brt_df`. Flagged rather than fixed.
#'
#' @section BRT coefficient naming:
#' The BRT coefficient is looked up as `"brt"` here, but because `brt` is a
#' logical column the model reports it as `"brtTRUE"` (which is the name
#' [check_coefficients()] uses). If the BRT factor ever comes back as exactly 1
#' when it shouldn't, this naming mismatch is the first place to look.
#'
#' @keywords internal
#' @noRd
forecast_ridership <- function(coefs,
                              data_xlsx,
                              acs_data,
                              gas_data,
                              scenario_inputs_df,
                              start_year = NULL,
                              start_month = NULL,
                              fare_df = NULL,
                              brt_df = NULL,
                              five_year = FALSE){

  #TODO: make it able to output a 5-year forecast.

  # rebuild the same modeling data frame the coefficients were estimated on,
  # so the reference ridership is on the identical scale
  df_all_log <- make_model_data_frame(data_xlsx, acs_data, gas_data, fare_df)

  min_year <- min(df_all_log$year) # must match the centering used at estimation

  # --- split the coefficient vector into its functional pieces ---

  # the true elasticities: everything except the seasonality dummies, the time
  # trend terms, and the three variables that were never log transformed
  elasticities <- coefs[!grepl("factor\\(month\\)|year_cent|workers_16_over|median_earnings|emp_pop_ratio", names(coefs))]

  # display label -> model variable lookup, used to recode the scenario table
  elast_names <- get_elasticity_varaibles(coefs)

  # get month coefficients if they are part of the model
  if (sum(grepl("factor\\(month\\)", names(coefs))) > 0) {

    month_coefs <- coefs[grepl("factor\\(month\\)", names(coefs))]
    # strip the "factor(month)" prefix so names are plain month numbers
    names(month_coefs) <- gsub("factor\\(month\\)", "", names(month_coefs))

    # add January's month coefficient explicitly as a zero
    # (January is the omitted reference level, so its effect is 0 by construction)
    month_coefs <- c("1" = 0, month_coefs)
  } else{
    # no seasonality in the model: every month gets a zero effect
    month_coefs <- c("1" = 0, "2" = 0,"3" = 0,"4" = 0,"5" = 0,"6" = 0,"7" = 0,"8" = 0,"9" = 0,"10" = 0,"11" = 0,"12" = 0)
  }

  # extract linear and quadratic time trend component coefficients
  # (default to 0 so the corresponding growth factor becomes exp(0) = 1)
  year_lin_coef <- ifelse("year_cent" %in% names(coefs),coefs["year_cent"], 0)
  year_quad_coef <- ifelse("I(year_cent^2)" %in% names(coefs),coefs["I(year_cent^2)"],0)

  # get the brt ceofficient if it exists
  # (see the naming note in the roxygen block above)
  brt_coef <- ifelse("brt" %in% names(coefs), coefs[names(coefs) == "brt"], 0)

  # get the year that the forecast will start from
  if (is.null(start_year)){
    ref_year <- max(df_all_log$year)
  } else{
    ref_year <- start_year
  }

  # get the month that the forecast will start from
  # default: the most recent month observed within the reference year
  if (is.null(start_month)){
    ref_month <- df_all_log |>
      filter(year == ref_year) |>
      select(month) |>
      unlist() |>
      as.numeric() |>
      max()
  } else{
    ref_month <- start_month
  }

  # reference UPT for all routes in the given year and month
  # exp() undoes the log transform, returning ridership to its natural scale.
  # This observed anchor is what makes the route fixed effects unnecessary.
  route_reference <- df_all_log |>
    filter(year == ref_year, month == ref_month) |>
    group_by(route_id) |>
    summarise(
      ref_ridership = mean(exp(log_upt_avg), na.rm = TRUE),
      .groups = "drop"
    )

  # prepare the scenario data frame for next step
  # the user supplies ANNUAL percent changes, so divide by 12 to get a monthly rate
  scenario_df <- organize_scenario_df(scenario_inputs_df) |>
    mutate(month_change = change / 12)


  if (five_year){
    # Create forecast grid until December in five years
    # one row per route x month x scenario, dropping months already observed
    forecast_grid <- expand_grid(
      route_id = unique(route_reference$route_id),
      year = ref_year:(ref_year + 5),
      month = 1:12,
      scenario = c("Low","Medium","High")) |>
      filter(!(year == ref_year & month <= ref_month)) |>
      left_join(route_reference, by = "route_id")

  } else{
    # Create forecast grid until December of the next year
    # one row per route x month x scenario, dropping months already observed
    forecast_grid <- expand_grid(
      route_id = unique(route_reference$route_id),
      year = ref_year:(ref_year + 1),
      month = 1:12,
      scenario = c("Low","Medium","High")) |>
      filter(!(year == ref_year & month <= ref_month)) |>
      left_join(route_reference, by = "route_id")
  }


  # get a column counting months from ref_month
  # months_from_ref drives how much cumulative change has accrued by that month
  forecast_grid <- forecast_grid |>
    mutate(date = ym(paste(year, month,sep = "-")),
           ref_date = ym(paste(ref_year, ref_month,sep = "-")),
           months_from_ref = time_length(interval(ref_date, date), unit = "month")) |>
    select(!c("date","ref_date"))

  # add brt column to forecast_grid
  if (!is.null(brt_df)){
    forecast_grid$brt <- 0
    forecast_grid$month_numeric <- as.numeric(forecast_grid$month)

    for (row_id in 1:nrow(brt_df)){
      date_used <- as.character(ymd(brt_df$change_date_brt[[row_id]]))
      brt_route <- as.numeric(brt_df$routes_brt[[row_id]])

      # update the brt with a 1 on the dates after the brt change
      forecast_grid <- forecast_grid |>
        dplyr::mutate(brt = ifelse(lubridate::ym(paste(year, month_numeric)) >= date_used, 1, brt))


    }
    forecast_grid$month_numeric <- NULL
  } else{
    forecast_grid$brt <- 0
  }

  #' add scenarios to forecast_grid
  #' and get elasticity factors
  forecast_expanded <- forecast_grid |>
    # many-to-many because each route-month row gets every scenario variable
    left_join(scenario_df, by = "scenario", relationship = "many-to-many") |>

    # need to change the variable names to match the elasticity names
    # (display labels like "VRM" -> model names like "log_vrm")
    mutate(variable = recode(variable, !!!elast_names)) |>
    mutate(
      # cumulative proportional change in the driver by this month
      total_month_change = months_from_ref*month_change,
      elasticity = recode(variable, !!!elasticities),
      # the core elasticity relationship: a proportional change in a driver
      # produces a (1 + change)^elasticity proportional change in ridership
      elasticity_factor = (1 + total_month_change)**elasticity
    ) |>
    select(!c("change", "month_change", "total_month_change", "elasticity")) |>
    # make each variable factor it's own row
    # (actually the reverse: back to one ROW per route-month-scenario, with one
    # factor_<variable> COLUMN per driver, ready to be multiplied together)
    pivot_wider(names_from = "variable",
                values_from = "elasticity_factor",
                names_prefix = "factor_")

  # incorporate year effect, month (seasonal) effect to get the total_log_change and forecasted UPT
  # every term below is expressed as a multiplicative growth factor, so that a
  # variable absent from the model contributes exp(0) = 1 and changes nothing
  forecast_factors <- forecast_expanded %>%
    mutate(
      # year centering
      # recreate the same centering used when the model was fit
      yc_target = year - min_year,
      yc_ref = ref_year - min_year,

      # year change factor
      factor_year = exp((yc_target - yc_ref)*year_lin_coef),

      # quadratic time trend change factor
      factor_year_quad = exp((yc_target^2 - yc_ref^2)*year_quad_coef),

      # monthly seasonality change factor
      # measured RELATIVE to the reference month, since the anchor already
      # contains the reference month's own seasonal effect
      month_coef = ifelse(
        month == ref_month,
        0,
        month_coefs[as.character(month)] -
          month_coefs[as.character(ref_month)]
      ),
      factor_month = exp(month_coef),

      # brt change factor
      # if brt == 0, the growth factor will be one
      # if brt == 1, the growth factor will depend on the value of the coefficient
      factor_brt = exp(brt*brt_coef),

      # a variable to say it is forecasted
      forecast = TRUE
    )

  # Get the name of the column containing "multiply"
  # (every growth factor column is prefixed "factor_", so grab them all)
  target_col <- grep("factor", names(forecast_factors), value = TRUE)

  # Add a new column 'product_result' to the dataframe
  # total growth = product of all the individual factors; rowwise() so the
  # product is taken across columns within each row
  forecast_full <- forecast_factors |>
    rowwise() |>
    mutate(total_growth = prod(c_across(all_of(target_col)))) |>
    ungroup() |>
    mutate(avg_daily_upt = ref_ridership * total_growth) |>
    select(route_id, year, month, scenario, forecast, avg_daily_upt)

  # combine forecasts and observed upt into one big df
  # observed rows are labelled scenario = "Observed" so they plot as their own line
  final_df <- df_all_log |>
    mutate(avg_daily_upt = exp(log_upt_avg),
           scenario = "Observed",
           forecast = FALSE,
           month = as.integer(month)) |>
    select(route_id, year, month, scenario, forecast, avg_daily_upt) |>
    bind_rows(forecast_full)

  # create a new route that combines the upt of all the routes
  # this synthetic "all_routes" is the system total shown by default on tab 7
  new_route_df <- final_df |>
    group_by(year, month, scenario, forecast) |>
    summarize(route_id = "all_routes",
              avg_daily_upt = sum(avg_daily_upt, na.rm = T),
              .groups = "drop_last")

  final_df <- bind_rows(final_df, new_route_df)

  # make a data frame with the reference month upt for all the scenarios
  # just to make the lines connect when we graph it
  # (without this each scenario line would start a month after the observed
  # line ends, leaving a visible gap in the chart)
  ref_month_upt <- final_df |>
    filter(month == ref_month,
           year == ref_year) |>
    select(route_id, avg_daily_upt) |>
    mutate(forecast = TRUE)

  ref_upt_df <- expand_grid(route_id = unique(final_df$route_id),
                            year = ref_year,
                            month = ref_month,
                            scenario = c("Low","Medium","High")) |>
    left_join(ref_month_upt, by = "route_id")


  new_final_df <- final_df |>
    bind_rows(ref_upt_df) |>
    # convert the average weekday figure back into a monthly total
    count_weekdays() |>
    mutate(tot_weekday_upt = avg_daily_upt * weekdays_in_month)|>
    # a real Date column makes plotting and export much easier
    mutate(date = ym(paste(year, month,sep = "/")))  |>
    select(route_id, year, month, avg_daily_upt,tot_weekday_upt, forecast, scenario, date)

  return(new_final_df)
}
