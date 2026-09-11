# get_files_to_zip.R ------------------------------------------------------
# Helper behind the "Download Plots (.zip)" button on the Visualization tab.

#' Write one forecast plot per route to a temporary directory
#'
#' Loops over the requested routes, renders the forecast plot for each one with
#' [plot_forecast()], saves it as a PNG in the session's temporary directory,
#' and returns the file paths. The paths are handed to `utils::zip()` inside the
#' `download_plots` download handler in `app_server()`, which bundles them into
#' a single zip archive for the user.
#'
#' @param forecast_df The forecast data frame created by
#'   [forecast_ridership()]. Must contain `route_id`, `date`, `avg_daily_upt`,
#'   and `scenario` columns.
#' @param routes A character vector of route IDs to plot. Each value must exist
#'   in `forecast_df$route_id`. The app passes every route plus `"all_routes"`.
#'
#' @returns A character vector of full file paths to the PNG files that were
#'   written. Files live in `tempdir()` and are cleaned up when the R session
#'   ends.
#'
#' @examples
#' \dontrun{
#' paths <- get_files_to_zip(forecast_df, c("1", "2", "all_routes"))
#' utils::zip(zipfile = "plots.zip", files = paths, flags = "-j")
#' }
#'
#' @keywords internal
#' @noRd
get_files_to_zip <- function(forecast_df, routes){

  # session-scoped scratch space; files here are removed when R exits
  temp_dir <- tempdir()

  files_to_zip <- c() # accumulator for the file paths we create

  for (route in routes){

    # build the ggplot for this single route (average weekday UPT by default)
    plot <- plot_forecast(forecast_df, route)

    # one PNG per route, named so the route is identifiable in the zip
    filepath <- file.path(temp_dir, paste0("plot_ridership_",route,".png"))
    ggsave(filepath, plot = plot, device = "png", width = 7, height = 5)

    files_to_zip <- c(files_to_zip, filepath)
  }

  return(files_to_zip)

}
