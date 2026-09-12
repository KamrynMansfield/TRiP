# create_regression_model_forced.R ----------------------------------------
# Produces the "Alternative Model" shown on tab 4. Same specification as
# create_regression_model() but with no stepwise elimination: it fits exactly
# the variables the user picked in the selectize input, significant or not.

#' Create a regression model with a forced set of variables
#'
#' Fits the same route fixed-effects log-log ridership model as
#' [create_regression_model()], but keeps every variable the user selected
#' instead of pruning by p-value. This backs the "Create New Model" button on
#' the Model Creation tab, letting an agency override the automatic selection
#' with variables they believe matter in their local context.
#'
#' The specification is `log_upt_avg ~ <variables> | route_id` with standard
#' errors clustered by route.
#'
#' @param data_xlsx Agency-provided ridership data as a data frame (the parsed
#'   contents of the uploaded .xlsx file).
#' @param acs_data Route-level monthly ACS data, as returned by
#'   [create_final_acs_data()].
#' @param gas_csv Monthly gas price data with `month`, `year`, and `gas_price`
#'   columns. Despite the name this is a data frame, not a file path; it is
#'   passed straight through to [make_model_data_frame()].
#' @param variables A character vector of variable names to include in the
#'   model. All of them are kept regardless of significance.
#' @param fare_df Optional data frame of adult base fare changes, with
#'   `change_date`, `prev_fare`, and `new_fare` columns. Default `NULL`.
#' @param brt_df Optional data frame of BRT conversions. Currently accepted but
#'   see the note below.
#'
#' @returns An object of class `fixest` containing the fitted model.
#'
#' @section Argument mismatches worth checking:
#' Two signature issues will surface as soon as these paths are exercised:
#' \enumerate{
#'   \item `app_server()` calls this function with `gas_data = gas`, but the
#'     parameter here is named `gas_csv`. R does not partially match a supplied
#'     name to a different formal, so that call errors with "unused argument".
#'     Renaming this parameter `gas_data` (matching [create_regression_model()])
#'     would align the two.
#'   \item `make_model_data_frame()` takes four arguments
#'     (`data_xlsx`, `acs_data`, `gas_data`, `fare_df`), but the call below
#'     passes five by adding `brt_df`. That errors whenever this function runs.
#'     Either drop `brt_df` from the call or add a `brt_df` parameter to
#'     `make_model_data_frame()`.
#' }
#' Flagged rather than fixed, since changing the code was out of scope.
#'
#' @seealso [create_regression_model()] for the stepwise version.
#' @keywords internal
#' @noRd
create_regression_model_forced <- function(data_xlsx,
                                           acs_data,
                                           gas_data,
                                           variables,
                                           fare_df = NULL,
                                           brt_df = NULL){

  # assemble and log-transform the joined ridership / ACS / gas data
  # (see the note above: this passes five arguments to a four-argument function)
  df_all_log <- make_model_data_frame(data_xlsx, acs_data, gas_data, fare_df)

  # changing the reference monthe to December (just for now)
  # TODO: Delet this eventually
  # df_all_log$month <- relevel(factor(df_all_log$month), ref = "12")

  candidate_variables <- variables

  # single fit, no elimination loop: every selected variable is kept.
  # `| route_id` absorbs a per-route intercept; cluster = ~route_id allows for
  # correlated errors within a route over time.
  lm_vrm <- feols(as.formula(paste("log_upt_avg~", paste(candidate_variables, collapse = " + "), "| route_id")), data=df_all_log, cluster= ~route_id)

  return(lm_vrm)
}
