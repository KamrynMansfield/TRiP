# get_elasticity_varaibles.R ----------------------------------------------
# NOTE: the file and function name contain a typo ("varaibles"). Left as-is
# because forecast_ridership() and app_server() both call it by that name;
# renaming would need to happen in all three places at once.
#
# Bridges the model and the forecasting UI: turns the coefficient names of the
# chosen model into the labelled variable list the user edits on tab 6, and is
# also used inside forecast_ridership() to translate those labels back.

#' Map model coefficients to display names for the scenario table
#'
#' Filters the friendly-name lookup down to just the variables that actually
#' appear in the fitted model, so the scenario input table on the Forecasting
#' Inputs tab only asks the user for percent changes in variables the model can
#' use. When no model is supplied it falls back to published default
#' elasticities for VRM and gas price.
#'
#' @param model_coefs A named numeric vector of model coefficients, such as
#'   `coef(selected_model())` or the output of the `final_coefs()` reactive. If
#'   `NULL`, literature default elasticities are used instead.
#' @param addnl_vars A named character vector of any extra variables the agency
#'   uploaded, in `c("Display Name" = "log_variable")` form, appended to the
#'   lookup so custom columns appear in the scenario table.
#'
#' @returns A named character vector of the subset of variables present in the
#'   model, where the names are the display labels and the values are the model
#'   variable names. Passed to `recode()` in [forecast_ridership()] and used to
#'   build the scenario table in `app_server()`.
#'
#' @section Note on the BRT filter:
#' BRT is deliberately excluded, because asking for a "percent increase in BRT"
#' makes no sense; the app collects BRT conversions as route/date pairs on tab 5
#' instead. Be aware that `model_coefs[model_coefs != "brt"]` filters on the
#' coefficient VALUES rather than their names, so it compares numbers to the
#' string "brt" and never actually drops anything.
#' `model_coefs[names(model_coefs) != "brt"]` would be the name-based version.
#' In practice `brt` is filtered out again downstream by the `%in%` check, so
#' this is more of a tidiness issue than a live bug. (Code left unchanged.)
#'
#' @keywords internal
#' @noRd
get_elasticity_varaibles <- function(model_coefs = NULL, addnl_vars = NULL){

  if (is.null(model_coefs)){
    # fallback defaults drawn from the transit elasticity literature
    elasticities <- c(log_vrm = .23,
               log_gas_price = .5)
  } else{
    # see the note above: this filters on values, not names
    elasticities <- model_coefs[model_coefs != "brt"]
  }

  # full display-label lookup; names are what the user sees in the scenario table
  nice_names <- c("VRM" = "log_vrm",
                  "Gas Price" = "log_gas_price",
                  "% No Vehicle Households" = "log_perc_hshlds_noveh",
                  "% Workers Below Federal Poverty Line" = "log_below_fpl",
                  "% Commuting by Car" = "log_perc_car",
                  "% Commuting by Taxi" = "log_perc_taxicab",
                  "% Work From Home" = "log_perc_wfh",
                  "% Female Workers" = "log_perc_female",
                  "% Workers Between 100-150% of Federal Povery Level" = "log_fpl_100_150",
                  "% Workers in Renter Occupied Housing Units" = "log_perc_renter_occupied",
                  "Labor Participation Rate" = "log_labor_part_rate",
                  "Unemployment Rate" = "log_unemp_rate",
                  "Fares" = "log_fare",
                  # "BRT" = "brt", # it doesn't make sense to estimate the percent increase in BRT, so I have created another way on the previous tab to just input which routes will be converted to BRT.
                  addnl_vars)

  # keep only the labels whose variable is actually in this model
  elast_names <- nice_names[nice_names %in% names(elasticities)]

  return(elast_names)

}
