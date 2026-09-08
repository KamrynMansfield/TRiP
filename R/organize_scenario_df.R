organize_scenario_df <- function(scenario_inputs_df){

  new_scenario_inputs_df <- data.frame(variable = scenario_inputs_df$Variable,
                                       Low = scenario_inputs_df$Low.Estimate,
                                       Mid = scenario_inputs_df$Mid.Estimate,
                                       High = scenario_inputs_df$High.Estimate)


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
    mutate(Low = as.numeric(gsub("%", "", Low)) / 100,
           Medium = as.numeric(gsub("%", "", Mid)) / 100,
           High = as.numeric(gsub("%", "", High)) / 100) |>
    select(variable, Low, Medium, High) |>
    pivot_longer(cols = c("Low", "Medium", "High"), names_to = "scenario", values_to = "change")

  return(scenario_df)

}
