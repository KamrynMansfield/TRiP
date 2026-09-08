get_files_to_zip <- function(forecast_df, routes){


  temp_dir <- tempdir()

  files_to_zip <- c()
  for (route in routes){

    plot <- plot_forecast(forecast_df, route)

    filepath <- file.path(temp_dir, paste0("plot_ridership_",route,".png"))
    ggsave(filepath, plot = plot, device = "png", width = 7, height = 5)

    files_to_zip <- c(files_to_zip, filepath)
  }

  return(files_to_zip)

}
