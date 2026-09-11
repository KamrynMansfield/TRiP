# get_fe.R ----------------------------------------------------------------
# Small helper for pulling estimated fixed effects out of a fixest model.

#' Extract fixed effect estimates as a data frame
#'
#' `feols()` models in the TRiP app absorb a `route_id` fixed effect, which
#' means each route gets its own intercept that is not reported in
#' `coef(model)`. This helper pulls those absorbed intercepts out of the model
#' and returns them in a tidy two-column data frame so they can be joined,
#' plotted, or inspected.
#'
#' @param model A model object of class `fixest`, such as the output of
#'   `create_regression_model()` or `create_regression_model_forced()`.
#' @param fe_name The name of the fixed effect dimension to extract, as a
#'   string. In this app that is almost always `"route_id"`.
#'
#' @returns A data frame with one row per group and two columns:
#'   \describe{
#'     \item{group}{The level of the fixed effect (e.g. the route ID).}
#'     \item{effect}{The estimated fixed effect (intercept shift) for that group.}
#'   }
#'
#' @examples
#' \dontrun{
#' mod <- create_regression_model(data_xlsx, acs_data, gas, variables)
#' get_fe(mod, "route_id")
#' }
#'
#' @keywords internal
#' @noRd
# pulls the fixed effects coefficients
get_fe <- function(model, fe_name) {
  # fixest::fixef() returns a list with one element per absorbed dimension
  all_fe <- fixef(model)

  # keep only the dimension the caller asked for (a named numeric vector)
  fe <- all_fe[[fe_name]]

  # the names of that vector are the group levels (e.g. each route_id)
  group_names <- names(fe)

  df <- data.frame( # create df with dynamic column names
    group = group_names,
    effect = as.numeric(fe),
    row.names = NULL)

  return(df)
}
