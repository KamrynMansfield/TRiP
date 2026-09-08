#' Plat forecasted UPT
#'
#' @param forecast_df The data frame with forecasted transit trips created from
#' the forecast_ridership() function
#' @param route the name of the route you want to see. It must exist in the forecast_df
#' @param scale either "average" or "total" to specify whether you want the y axis to have
#' average weekday UPT or total weekday UPT for the month.
#'
#' @returns
#' @export
#'
#' @examples
plot_forecast <- function(forecast_df, route = "all_routes", scale = "average"){

  if (route == "all_routes"){
    plot_title <- paste("Unlinked Passenger Trips (UPT) Forecast - Summed Ridership for All Routes")
  } else{
    plot_title <- paste("Unlinked Passenger Trips (UPT) Forecast - Route:", route)
  }

  if(scale == "average"){
    forecast_df |>
      filter(route_id == route) |>
      ggplot() +
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
