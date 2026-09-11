# check_coefficients.R ----------------------------------------------------
# Powers the "Selected model" table on tab 5 (Model Review). Its job is to make
# implausible coefficient signs visually obvious before the user commits to a
# model and forecasts with it.

#' Check model coefficient reasonableness
#'
#' Builds a formatted coefficient table for the chosen model and colors any row
#' whose sign contradicts theory or prior research: red for an error (a sign
#' that should never occur, such as a negative VRM elasticity, which would mean
#' running more service reduces ridership) and yellow for a warning (a sign
#' worth a second look). Rows with no expectation on file are left uncolored.
#'
#' @param model A model created to forecast bus ridership in the TRiP app,
#'   typically the `fixest` object stored in `selected_model()`.
#' @param extra_vars A named character vector of any additional variables the
#'   agency uploaded, in `c("Display Name" = "log_variable")` form, as produced
#'   by the `addnl_vars()` reactive in `app_server()`. These are appended to the
#'   lookup so custom columns get readable labels instead of `NA`.
#'
#' @returns A `gt` table of coefficients that is colored to point out unexpected
#'   signs. The helper `sign_check` column is hidden before rendering, and the
#'   `Message` column only appears when at least one variable triggered a flag.
#' @export
#'
#' @examples
#' \dontrun{
#' check_coefficients(selected_model(), addnl_vars())
#' }
check_coefficients <- function(model, extra_vars){
  # lookup from raw coefficient name -> human readable label.
  # names(potential_coeff) are the display labels, the values are the
  # coefficient names fixest reports.
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
                       extra_vars) # agency-supplied variables appended last



  # this df will be used later to check the signs
  # I can add more to this list as time goes on
  # Each row is: variable, the sign theory expects, how loudly to complain,
  # and the message shown to the user. Variables absent from this table are
  # never flagged. Note brtTRUE appears twice (once per sign) so that BRT always
  # draws a warning about its thin sample regardless of which way it points.
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
    # attach the expectation (NA for variables we have no prior about)
    left_join(sign_df, by = "variable") |>
    mutate(actual_sign = ifelse(coeff >= 0, "positive","negative")) |>
    mutate(sign_check = case_when(
      is.na(expected_sign) ~ "sign_ok",           # no expectation on file
      expected_sign == actual_sign ~ "sign_ok",   # matches expectation
      TRUE ~ label                                # mismatch: "error" or "warning"
    )) |>
      # only surface the explanatory text on rows that actually tripped a check
      mutate(new_message = ifelse(sign_check == "sign_ok", "",message))

  # swap raw coefficient names for the display labels defined above
  coef_df$variable_name <- names(potential_coeff)[match(coef_df$variable,potential_coeff)]

  # if every row is "sign_ok" there is only one distinct (empty) message, so the
  # Message column is dropped to keep the table narrow
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
      # the flag column is only a styling helper, so hide it from the user
      cols_hide(columns = sign_check)
}
