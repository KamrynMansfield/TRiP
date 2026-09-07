#' Run the Shiny Application
#'
#' Launches the application in the user's default browser.
#'
#' @param ... Arguments passed to [shiny::shinyApp()], such as `options`.
#'
#' @return A Shiny app object. Called for its side effect of launching the app.
#'
#' @examples
#' \dontrun{
#' run_app()
#' }
#'
#' @export
run_app <- function(...) {
  shiny::shinyApp(
    ui     = app_ui(),
    server = app_server,
    ...
  )
}
