create_regression_model_forced <- function(data_xlsx,
                                           acs_data,
                                           gas_csv,
                                           variables,
                                           fare_df = NULL,
                                           brt_df = NULL){

  df_all_log <- make_model_data_frame(data_xlsx, acs_data, gas_csv, fare_df, brt_df)

  # changing the reference monthe to December (just for now)
  # TODO: Delet this eventually
  # df_all_log$month <- relevel(factor(df_all_log$month), ref = "12")

  candidate_variables <- variables

  lm_vrm <- feols(as.formula(paste("log_upt_avg~", paste(candidate_variables, collapse = " + "), "| route_id")), data=df_all_log, cluster= ~route_id)

  return(lm_vrm)
}
