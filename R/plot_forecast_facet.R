#' Plot forecasted UPT for all routes in a facet wrap
#'
#' @param forecast_df The data frame with forecasted transit trips created from 
#' the forecast_ridership() function
#' @param scale either "average" or "total" to specify whether you want the y axis to have
#' average weekday UPT or total weekday UPT for the month. 
#'
#' @returns
#' @export
#'
#' @examples
plot_forecast_facet <- function(forecast_df, scale = "average"){
  
  plot_title <- paste("Unlinked Passenger Trips (UPT) Forecast")
  
  if(scale == "average"){
    forecast_df |>
      ggplot() +
      geom_line(aes(x = date, y = avg_daily_upt, color = scenario)) +
      facet_wrap(~route_id, scales = "free") +
      theme_bw() +
      labs(x = "Date",
           y = "UPT (Average Weekday)",
           color = "Scenario",
           title = plot_title)
    
  }else if (scale == "total"){
    forecast_df |>
      ggplot() +
      geom_line(aes(x = date, y = tot_weekday_upt, color = scenario)) +
      facet_wrap(~route_id, scales = "free") +
      theme_bw() +
      labs(x = "Date",
           y = "UPT (Total Weekday)",
           color = "Scenario",
           title = plot_title)
  }else{
    stop('Scale must be "average" or "total"')
  }
  
  
}
