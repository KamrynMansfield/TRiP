#' Check model coefficient reasonableness
#'
#' @param model a model created to forecast bus ridership in the TRiP app.
#'
#' @returns A gt table of coefficients that is colored to point out unexpected signs
#' @export
#'
#' @examples
check_coefficients <- function(model, extra_vars){
  potential_coeff <- c("VRM" = "log_vrm",
                       "February" = "factor(month)2",
                       "March" = "factor(month)3",
                       "April" = "factor(month)4",
                       "May" = "factor(month)5",
                        "June" = "factor(month)6",
                        "July" = "factor(month)7",
                        "August" = "factor(month)8",
                        "September" = "factor(month)9",
                        "October" = "factor(month)10",
                        "November" = "factor(month)11",
                       "December" = "factor(month)12",
                           "Year" = "year_cent",
                           "Year Squared" = "I(year_cent^2)",
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
                       "Is Bus Rapid Transit" = "brtTRUE",
                       "Fare" = "log_fare",
                       extra_vars)



  # this df will be used later to check the signs
  # I can add more to this list as time goes on
  sign_df <- matrix(c("log_vrm", "positive", "error", "The VRM coefficient must be positive.",
                      "log_gas_price", "positive", "warning", "Positive sign expected, use best judgement",
                      "log_perc_hshlds_noveh", "positive", "warning", "Positive sign expected, use best judgement",
                      "log_perc_car", "positive", "warning", "Positive sign expected, use best judgement",
                      "log_perc_wfh", "positive", "warning", "Positive sign expected, use best judgement",
                      "brtTRUE",  "positive", "warning", "Because so few routes are BRT, this may not be as statistically significant as the p-value lets on. Use best judgement",
                      "brtTRUE",  "negative", "warning", "Because so few routes are BRT, this may not be as statistically significant as the p-value lets on. Use best judgement",
                      "log_fare", "negative", "warning", "Negative sign expected, use best judgement"),
                    byrow = T, ncol = 4) |>
    as.data.frame()

  names(sign_df) <- c("variable", "expected_sign","label","message")




  coefs <- coef(model)
  # coefs <- c(log_perc_car = -.45, log_vrm = -.5) # This is just to check my code

  coef_df <- data.frame(variable = names(coefs),
                        coeff = round(coefs,3)) |>
    left_join(sign_df, by = "variable") |>
    mutate(actual_sign = ifelse(coeff >= 0, "positive","negative")) |>
    mutate(sign_check = case_when(
      is.na(expected_sign) ~ "sign_ok",
      expected_sign == actual_sign ~ "sign_ok",
      TRUE ~ label
    )) |>
      mutate(new_message = ifelse(sign_check == "sign_ok", "",message))

  coef_df$variable_name <- names(potential_coeff)[match(coef_df$variable,potential_coeff)]

  if(length(unique(coef_df$new_message)) == 1){
    coef_df <- coef_df |>
      select("Variable" = variable_name, "Coeff" = coeff, sign_check)
  } else{
    coef_df <- coef_df |>
      select("Variable" = variable_name, "Coeff" = coeff,"Message" = new_message, sign_check)
  }

  coef_df |>
    gt() |>
    # Color rows yellow where sign_check is warning
    tab_style(
      style = cell_fill(color = "yellow"),
      locations = cells_body(rows = sign_check == "warning")
    ) |>
    # Color rows red where sign_check is error
    tab_style(
      style = cell_fill(color = "red"),
      locations = cells_body(rows = sign_check == "error")
    ) |>
      cols_hide(columns = sign_check)
}




