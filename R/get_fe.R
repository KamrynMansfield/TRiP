# pulls the fixed effects coefficients
get_fe <- function(model, fe_name) {
  all_fe <- fixef(model)
  fe <- all_fe[[fe_name]]
  group_names <- names(fe)
  df <- data.frame( # create df with dynamic column names
    group = group_names,
    effect = as.numeric(fe),
    row.names = NULL)

  return(df)
}
