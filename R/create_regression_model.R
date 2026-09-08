#' Create regression model to predict bus ridership
#'
#' This is a main function of the TRiP tool. It uses agency-level ridership data and external
#' census data to create a fixed effects regression model that can help
#' predict bus ridership. This function uses a backwards stepwise method
#' to create a model with only coefficients that have a p-value less than 0.1.
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
#' @returns Results of the regression model. An R oject of class "fixest"
#' @export
#'
#' @examples
create_regression_model <- function(data_xlsx,
                                    acs_data,
                                    gas_data,
                                    variables,
                                    fare_df = NULL){

  df_all_log <- make_model_data_frame(data_xlsx, acs_data, gas_data, fare_df)

  # changing the reference monthe to December (just for now)
  # TODO: Delet this eventually
  # df_all_log$month <- relevel(factor(df_all_log$month), ref = "12")

  candidate_variables <- variables

  rejected_var <- "placeholder"
  max_p <- .9
  n_iter <- 0
  max_iter <- length(candidate_variables) + 5

  while (max_p > 0.1){
    # exclude the previous iteration's high p-value variable
    candidate_variables <- candidate_variables[!candidate_variables %in% rejected_var]

    if (length(candidate_variables) == 0) stop("No candidate variables left to fit.")

    # create the model
    lm_vrm <- feols(as.formula(paste("log_upt_avg~", paste(candidate_variables, collapse = " + "), "| route_id")), data=df_all_log, cluster= ~route_id)

    # get vector of p-values
    pvals <- lm_vrm$coeftable[,"Pr(>|t|)"]

    # get the max p-value
    max_p <- max(pvals, na.rm = TRUE)
    if (!is.finite(max_p)) break

    # get the name of the variable with the highest p-value
    # this one will get kicked out next iteration
    rejected_var <- names(pvals)[which.max(pvals)]

    if (rejected_var == "I(year_cent^2)"){
      rejected_var <- "year_cent^2"
    } else if (grepl("month",rejected_var, ignore.case = TRUE)){
      rejected_var <- "factor(month)"
    }

    n_iter <- n_iter + 1

    if (n_iter > max_iter){
      break
    }
  }

  return(lm_vrm)
}



