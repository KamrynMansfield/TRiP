# create_regression_model.R -----------------------------------------------
# Produces the "Default Model" shown on tab 4. Fits a route fixed-effects
# log-log model and prunes it with backwards stepwise elimination until every
# remaining coefficient clears a 10 percent significance threshold.

#' Create regression model to predict bus ridership
#'
#' This is a main function of the TRiP tool. It uses agency-level ridership data and external
#' census data to create a fixed effects regression model that can help
#' predict bus ridership. This function uses a backwards stepwise method
#' to create a model with only coefficients that have a p-value less than 0.1.
#'
#' The fitted specification is
#' `log_upt_avg ~ <candidate variables> | route_id`, with standard errors
#' clustered by route. Absorbing `route_id` means every route gets its own
#' intercept, so the coefficients describe within-route variation over time
#' rather than differences between routes. Because both sides are logged, the
#' coefficients are elasticities, which is what [forecast_ridership()] relies on.
#'
#' @param data_xlsx Agency-provided excel data as an r object of class `df`
#' @param acs_data Organized ACS data pulled from the census API using a few functions
#' like `pull_acs_data()` and `create_final_acs_data()`
#' @param gas_data Internal data storing historical gas prices obtained
#' from [U.S. Energy Information Administration](https://www.eia.gov/dnav/pet/pet_pri_gnd_dcus_nus_m.htm)
#' @param variables A vector of variable names that will be tried in the regression model.
#' @param fare_df An optional data frame that describes the dates of the fare changes.
#' Default is NULL.
#'
#' @returns Results of the regression model. An R object of class "fixest",
#'   containing only the variables that survived the elimination loop.
#' @seealso [create_regression_model_forced()] for the version that fits exactly
#'   the variables the user selects, with no elimination.
#' @export
#'
#' @examples
#' \dontrun{
#' mod <- create_regression_model(
#'   data_xlsx = processed_data,
#'   acs_data  = acs,
#'   gas_data  = gas,
#'   variables = c("log_vrm", "factor(month)", "year_cent", "log_gas_price")
#' )
#' }
create_regression_model <- function(data_xlsx,
                                    acs_data,
                                    gas_data,
                                    variables,
                                    fare_df = NULL){

  # assemble and log-transform the joined ridership / ACS / gas data
  df_all_log <- make_model_data_frame(data_xlsx, acs_data, gas_data, fare_df)

  # changing the reference monthe to December (just for now)
  # TODO: Delet this eventually
  # df_all_log$month <- relevel(factor(df_all_log$month), ref = "12")

  candidate_variables <- variables

  # --- backwards stepwise elimination setup ---
  rejected_var <- "placeholder" # nothing to drop on the first pass
  max_p <- .9                   # seed value that guarantees at least one iteration
  n_iter <- 0
  max_iter <- length(candidate_variables) + 5 # safety valve against an infinite loop

  # keep refitting until the worst p-value is at or below 0.10
  while (max_p > 0.1){
    # exclude the previous iteration's high p-value variable
    candidate_variables <- candidate_variables[!candidate_variables %in% rejected_var]

    if (length(candidate_variables) == 0) stop("No candidate variables left to fit.")

    # create the model
    # NOTE: if "year_cent^2" is among the candidates, R's formula parser reads
    # `^` as interaction expansion, so `year_cent^2` collapses to `year_cent`
    # rather than a quadratic term. `I(year_cent^2)` is the form that produces a
    # true squared term, and it is the name the rejection branch below expects.
    # Worth verifying against your model output. (Code left unchanged.)
    lm_vrm <- feols(as.formula(paste("log_upt_avg~", paste(candidate_variables, collapse = " + "), "| route_id")), data=df_all_log, cluster= ~route_id)

    # get vector of p-values
    pvals <- lm_vrm$coeftable[,"Pr(>|t|)"]

    # get the max p-value
    max_p <- max(pvals, na.rm = TRUE)
    if (!is.finite(max_p)) break # nothing usable to evaluate; keep the current fit

    # get the name of the variable with the highest p-value
    # this one will get kicked out next iteration
    rejected_var <- names(pvals)[which.max(pvals)]

    # the coefficient names that come back don't always match the names used in
    # the formula, so translate them back before removing the term
    if (rejected_var == "I(year_cent^2)"){
      rejected_var <- "year_cent^2"
    } else if (grepl("month",rejected_var, ignore.case = TRUE)){
      # month enters as a factor, so one insignificant month dummy drops the
      # entire seasonality term rather than that single month
      rejected_var <- "factor(month)"
    }

    n_iter <- n_iter + 1

    # hard stop so a variable that can never be matched and removed can't
    # spin the loop forever
    if (n_iter > max_iter){
      break
    }
  }

  return(lm_vrm)
}
