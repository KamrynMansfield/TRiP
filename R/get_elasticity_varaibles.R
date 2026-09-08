get_elasticity_varaibles <- function(model_coefs = NULL, addnl_vars = NULL){

  if (is.null(model_coefs)){
    elasticities <- c(log_vrm = .23,
               log_gas_price = .5)
  } else{
    elasticities <- model_coefs[model_coefs != "brt"]
  }

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

  elast_names <- nice_names[nice_names %in% names(elasticities)]

  return(elast_names)

}

