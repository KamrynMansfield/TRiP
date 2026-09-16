## code to prepare `my_pkg_data` dataset goes here

# State Planes Data
load("inst/extdata/state_planes_crs.rda")

usethis::use_data(state_planes_crs, overwrite = TRUE, internal = TRUE)

# US Counties Data
us_counties <- readRDS("inst/extdata/us_counties.rds")

usethis::use_data(us_counties, overwrite = TRUE, internal = TRUE)

# Gas Price Data
gas <- readr::read_csv("inst/extdata/Midwest_All_Grades_All_Formulations_Retail_Gasoline_Prices.csv",
                skip = 4) |>
  dplyr::rename(date = "Month",
         gas_price = "Midwest All Grades All Formulations Retail Gasoline Prices Dollars per Gallon") |>
  dplyr::mutate(date = lubridate::my(date),
         month = lubridate::month(date),
         year = lubridate::year(date)) |>
  dplyr::select(-date)

usethis::use_data(gas, overwrite = TRUE, internal = TRUE)

