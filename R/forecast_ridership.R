# coefs <- final_coefs
# data_xlsx <- vrm_data
# acs_data <- acs
# gas_csv <- "data/Midwest_All_Grades_All_Formulations_Retail_Gasoline_Prices.csv"
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

forecast_ridership <- function(coefs,
                              data_xlsx,
                              acs_data,
                              gas_csv,
                              scenario_inputs_df,
                              start_year = NULL,
                              start_month = NULL,
                              fare_df = NULL,
                              brt_df = NULL){

  df_all_log <- make_model_data_frame(data_xlsx, acs_data, gas_csv, fare_df, brt_df)

  min_year <- min(df_all_log$year)

  elasticities <- coefs[!grepl("factor\\(month\\)|year_cent|workers_16_over|median_earnings|emp_pop_ratio", names(coefs))]

  elast_names <- get_elasticity_varaibles(coefs)

  # get month coefficients if they are part of the model
  if (sum(grepl("factor\\(month\\)", names(coefs))) > 0) {

    month_coefs <- coefs[grepl("factor\\(month\\)", names(coefs))]
    names(month_coefs) <- gsub("factor\\(month\\)", "", names(month_coefs))

    # add January's month coefficient explicitly as a zero
    month_coefs <- c("1" = 0, month_coefs)
  } else{
    month_coefs <- c("1" = 0, "2" = 0,"3" = 0,"4" = 0,"5" = 0,"6" = 0,"7" = 0,"8" = 0,"9" = 0,"10" = 0,"11" = 0,"12" = 0)
  }

  # extract linear and quadratic time trend component coefficients
  year_lin_coef <- ifelse("year_cent" %in% names(coefs),coefs["year_cent"], 0)
  year_quad_coef <- ifelse("I(year_cent^2)" %in% names(coefs),coefs["I(year_cent^2)"],0)

  # get the brt ceofficient if it exists
  brt_coef <- ifelse("brt" %in% names(coefs), coefs[names(coefs) == "brt"], 0)

  # get the year that the forecast will start from
  if (is.null(start_year)){
    ref_year <- max(df_all_log$year)
  } else{
    ref_year <- start_year
  }

  # get the month that the forecast will start from
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
  route_reference <- df_all_log |>
    filter(year == ref_year, month == ref_month) |>
    group_by(route_id) |>
    summarise(
      ref_ridership = mean(exp(log_upt_avg), na.rm = TRUE),
      .groups = "drop"
    )

  # prepare the scenario data frame for next step
  scenario_df <- organize_scenario_df(scenario_inputs_df) |>
    mutate(month_change = change / 12)


  # Create forecast grid until December of the next year
  forecast_grid <- expand_grid(
    route_id = unique(route_reference$route_id),
    year = c(ref_year, ref_year + 1),
    month = 1:12,
    scenario = c("Low","Medium","High")) |>
    filter(!(year == ref_year & month <= ref_month)) |>
    left_join(route_reference, by = "route_id") |>

    # get a column counting months from ref_month
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
      month_used <- as.numeric(month(date_used))
      year_used <- as.numeric(year(date_used))
      brt_route <- as.numeric(brt_df$routes_brt[[row_id]])

      forecast_grid <- forecast_grid |>
        mutate(brt = case_when(
          month_numeric >= month_used & year >= year_used & route_id == brt_route ~ 1,
          TRUE ~ brt
        ))
    }
    forecast_grid$month_numeric <- NULL
  } else{
    forecast_grid$brt <- 0
  }

  #' add scenarios to forecast_grid
  #' and get elasticity factors
  forecast_expanded <- forecast_grid |>
    left_join(scenario_df, by = "scenario", relationship = "many-to-many") |>

    # need to change the variable names to match the elasticity names
    mutate(variable = recode(variable, !!!elast_names)) |>
    mutate(
      total_month_change = months_from_ref*month_change,
      elasticity = recode(variable, !!!elasticities),
      elasticity_factor = (1 + total_month_change)**elasticity
    ) |>
    select(!c("change", "month_change", "total_month_change", "elasticity")) |>
    # make each variable factor it's own row
    pivot_wider(names_from = "variable",
                values_from = "elasticity_factor",
                names_prefix = "factor_")

  # incorporate year effect, month (seasonal) effect to get the total_log_change and forecasted UPT
  forecast_factors <- forecast_expanded %>%
    mutate(
      # year centering
      yc_target = year - min_year,
      yc_ref = ref_year - min_year,

      # year change factor
      factor_year = exp((yc_target - yc_ref)*year_lin_coef),

      # quadratic time trend change factor
      factor_year_quad = exp((yc_target^2 - yc_ref^2)*year_quad_coef),

      # monthly seasonality change factor
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
  target_col <- grep("factor", names(forecast_factors), value = TRUE)

  # Add a new column 'product_result' to the dataframe
  forecast_full <- forecast_factors |>
    rowwise() |>
    mutate(total_growth = prod(c_across(all_of(target_col)))) |>
    ungroup() |>
    mutate(avg_daily_upt = ref_ridership * total_growth) |>
    select(route_id, year, month, scenario, forecast, avg_daily_upt)

  # combine forecasts and observed upt into one big df
  final_df <- df_all_log |>
    mutate(avg_daily_upt = exp(log_upt_avg),
           scenario = "Observed",
           forecast = FALSE,
           month = as.integer(month)) |>
    select(route_id, year, month, scenario, forecast, avg_daily_upt) |>
    bind_rows(forecast_full)

  # create a new route that combines the upt of all the routes
  new_route_df <- final_df |>
    group_by(year, month, scenario, forecast) |>
    summarize(route_id = "all_routes",
              avg_daily_upt = sum(avg_daily_upt, na.rm = T),
              .groups = "drop_last")

  final_df <- bind_rows(final_df, new_route_df)

  # make a data frame with the reference month upt for all the scenarios
  # just to make the lines connect when we graph it
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
    count_weekdays() |>
    mutate(tot_weekday_upt = avg_daily_upt * weekdays_in_month)|>
    mutate(date = ym(paste(year, month,sep = "/")))  |>
    select(route_id, year, month, avg_daily_upt,tot_weekday_upt, forecast, scenario, date)

  return(new_final_df)
}

