# organize_scenario_df.R --------------------------------------------------
# Reshapes the scenario table the user fills in on tab 6 into the long format
# forecast_ridership() expects, converting the typed percentages into decimals.

#' Reshape scenario inputs into long format
#'
#' Takes the wide scenario table produced by the Forecasting Inputs tab (one row
#' per variable, one column per scenario) and returns a long data frame with one
#' row per variable-scenario pair. Percent strings typed by the user (`"5%"`,
#' `"-1%"`) are stripped of their percent sign and divided by 100, so a `"5%"`
#' entry becomes `0.05`. `forecast_ridership()` then divides that annual change
#' by 12 to get a monthly rate.
#'
#' @param scenario_inputs_df The saved scenario table, with columns `Variable`,
#'   `Low.Estimate`, `Mid.Estimate`, and `High.Estimate`. Values may be numeric
#'   or percent-formatted strings.
#'
#' @returns A data frame with three columns:
#'   \describe{
#'     \item{variable}{The variable's display name (for example "VRM").}
#'     \item{scenario}{One of "Low", "Medium", or "High". Note the middle
#'       scenario is renamed from "Mid" to "Medium" here, matching the scenario
#'       labels used in the forecast grid and plot legends.}
#'     \item{change}{The assumed annual change as a decimal fraction.}
#'   }
#'
#' @keywords internal
#' @noRd
organize_scenario_df <- function(scenario_inputs_df){

  # rename the incoming columns to short, consistent names
  new_scenario_inputs_df <- data.frame(variable = scenario_inputs_df$Variable,
                                       Low = scenario_inputs_df$Low.Estimate,
                                       Mid = scenario_inputs_df$Mid.Estimate,
                                       High = scenario_inputs_df$High.Estimate)


  # display label -> model variable lookup. Currently unused: the recode from
  # label to variable name happens in forecast_ridership() via
  # get_elasticity_varaibles(), and the line applying this key is commented out
  # below. Kept here as a reference copy of the mapping.
  name_key <- c("VRM" = "log_vrm",
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
                  "BRT" = "brt")

  # new_scenario_inputs_df$variable <- unname(name_key[new_scenario_inputs_df$variable])


  scenario_df <- new_scenario_inputs_df |>
    # strip any "%" the user typed and convert to a decimal fraction,
    # renaming Mid -> Medium on the way through
    mutate(Low = as.numeric(gsub("%", "", Low)) / 100,
           Medium = as.numeric(gsub("%", "", Mid)) / 100,
           High = as.numeric(gsub("%", "", High)) / 100) |>
    select(variable, Low, Medium, High) |>
    # one row per variable-scenario combination
    pivot_longer(cols = c("Low", "Medium", "High"), names_to = "scenario", values_to = "change")

  return(scenario_df)

}
