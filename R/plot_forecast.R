# plot_forecast.R ---------------------------------------------------------
# Single-route forecast chart. Used for the plot on tab 7 (Visualization) and
# for every PNG written by get_files_to_zip() for the bulk download.

#' Plot forecasted UPT
#'
#' Draws observed and forecasted ridership for one route as a time series, with
#' one colored line per scenario. The observed history appears as the "Observed"
#' series and the three projected scenarios branch off from the reference month.
#'
#' @param forecast_df The data frame with forecasted transit trips created from
#' the forecast_ridership() function
#' @param route the name of the route you want to see. It must exist in the forecast_df.
#' Defaults to `"all_routes"`, the synthetic route that sums ridership across the
#' whole system.
#' @param scale either "average" or "total" to specify whether you want the y axis to have
#' average weekday UPT or total weekday UPT for the month.
#'
#' @returns A `ggplot` object.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_forecast(forecast_df)                       # system total
#' plot_forecast(forecast_df, route = "14")         # a single route
#' plot_forecast(forecast_df, scale = "total")      # monthly totals instead
#' }
plot_forecast <- function(forecast_df, route = "all_routes", scale = "average"){

  # title changes depending on whether this is the system total or one route
  if (route == "all_routes"){
    plot_title <- paste("Unlinked Passenger Trips (UPT) Forecast - Summed Ridership for All Routes")
  } else{
    plot_title <- paste("Unlinked Passenger Trips (UPT) Forecast - Route:", route)
  }

  # the two branches differ only in which y variable and axis label they use
  if(scale == "average"){
    forecast_df |>
      filter(route_id == route) |>
      ggplot() +
      # one line per scenario: Observed, Low, Medium, High
      geom_line(aes(x = date, y = avg_daily_upt, color = scenario)) +
      theme_bw() +
      labs(x = "Date",
           y = "UPT (Average Weekday)",
           color = "Scenario",
           title = plot_title)

  }else if (scale == "total"){
    forecast_df |>
      filter(route_id == route) |>
      ggplot() +
      # average weekday UPT multiplied by the number of weekdays in the month
      geom_line(aes(x = date, y = tot_weekday_upt, color = scenario)) +
      theme_bw() +
      labs(x = "Date",
           y = "UPT (Total Weekday)",
           color = "Scenario",
           title = plot_title)
  }else{
    stop('Scale must be "average" or "total"')
  }


}
