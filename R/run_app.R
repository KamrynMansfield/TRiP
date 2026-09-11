# run_app.R ---------------------------------------------------------------
# The single user-facing entry point of the package. Everything else in R/ is
# either a UI/server component or a helper called from app_server().

#' Run the Shiny Application
#'
#' Launches the TRiP forecasting application in the user's default browser.
#' The app walks a transit agency through eight tabs: screening, ridership data
#' upload, GTFS upload, model creation, model review, forecasting inputs,
#' visualization, and export.
#'
#' @param ... Arguments passed to [shiny::shinyApp()], such as `options`
#'   (for example `options = list(launch.browser = TRUE, port = 8080)`).
#'
#' @return A Shiny app object. Called for its side effect of launching the app.
#'
#' @seealso [app_ui()] and [app_server()] for the UI and server definitions.
#'
#' @examples
#' \dontrun{
#' run_app()
#' }
#'
#' @export
run_app <- function(...) {
  shiny::shinyApp(
    ui     = app_ui(),    # app_ui() is a function, so it is called here to build the tags
    server = app_server,  # app_server is passed by name; Shiny calls it per session
    ...
  )
}
