#' TRiP: Transit Ridership Prediction Tool
#'
#' A Shiny application that forecasts bus ridership for small- to medium-sized
#' transit agencies. TRiP combines historical route-level ridership with service
#' levels, demographic and economic indicators, and telecommuting rates to fit a
#' fixed-effects regression model and project ridership forward.
#'
#' @section Main workflow:
#' The app proceeds through the following stages, each backed by functions in
#' this package:
#' \enumerate{
#'   \item Load and screen agency ridership data
#'   \item Attach GTFS-derived service characteristics
#'   \item Join ACS demographics and gas price series
#'   \item Fit the regression model
#'   \item Forecast and visualize future ridership
#' }
#'
#' @section Getting started:
#' Launch the application with \code{\link{run_app}()}.
#'
#' @keywords internal
#'
#' @import shiny
#' @importFrom dplyr filter mutate select group_by summarize left_join arrange
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme_minimal
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats lm predict
#' @importFrom utils head read.csv
"_PACKAGE"
