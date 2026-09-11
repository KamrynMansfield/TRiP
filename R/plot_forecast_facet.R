# plot_forecast_facet.R ---------------------------------------------------
# All-routes-at-once version of plot_forecast(). Not currently wired into the
# UI, but useful for a quick overview of every route in one figure.

#' Plot forecasted UPT for all routes in a facet wrap
#'
#' Same chart as [plot_forecast()] but faceted, with one small panel per route.
#' Each panel gets free y and x scales, since ridership levels differ by orders
#' of magnitude between a trunk route and a coverage route and a shared scale
#' would flatten the smaller ones.
#'
#' @param forecast_df The data frame with forecasted transit trips created from
#' the forecast_ridership() function
#' @param scale either "average" or "total" to specify whether you want the y axis to have
#' average weekday UPT or total weekday UPT for the month.
#'
#' @returns A `ggplot` object with one facet per `route_id`.
#' @seealso [plot_forecast()] for the single-route version used in the app.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_forecast_facet(forecast_df)
#' plot_forecast_facet(forecast_df, scale = "total")
#' }
plot_forecast_facet <- function(forecast_df, scale = "average"){
  
  plot_title <- paste("Unlinked Passenger Trips (UPT) Forecast")
  
  # the two branches differ only in which y variable and axis label they use
  if(scale == "average"){
    forecast_df |>
      ggplot() +
      geom_line(aes(x = date, y = avg_daily_upt, color = scenario)) +
      # free scales because route ridership levels vary widely
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
