#' Get monthly U.S. retail gasoline prices from the EIA API
#'
#' See [EIA Open Data](https://www.eia.gov/opendata/) page for more info.
#'
#' @param start_month integer, 1-12
#' @param start_year integer, e.g. 2020
#' @param end_month integer, 1-12
#' @param end_year integer, e.g. 2024
#' @param region EIA duoarea code, e.g. "NUS" (U.S. avg), "R10"-"R50" (PADDs),
#'   or a state-level code. Defaults to U.S. average.
#' @param product EIA product code: "EPMR" = regular grade, "EPMM" = midgrade,
#'   "EPMP" = premium, "EPM0" = all grades. Defaults to all_grades.
#'
#' @return tibble with columns: gas_price, month, year, region
get_gas_data <- function(start_month,
                         start_year,
                         end_month,
                         end_year,
                         region = "NUS",
                         product = "EPM0") {

  start_date <- sprintf("%04d-%02d", start_year, start_month)
  end_date   <- sprintf("%04d-%02d", end_year, end_month)

  # Try pulling the data from the api. If the call fails, then NULL will be returned
  raw <- tryCatch({
    eia_data(
      dir     = "petroleum/pri/gnd",
      data    = "value",
      facets  = list(product = product, duoarea = region),
      freq    = "monthly",
      start   = start_date,
      end     = end_date,
      sort    = list(cols = "period", order = "asc")
    )

  }, error = function(e) {
    return(NULL)
  })

  # if data was successully pulled, organized it into
  # gas_price, month, and year columns.
  if(is.null(raw)){
    return(NULL)
  } else{
    raw |>
      mutate(
        period = ym(period),           # parse "YYYY-MM" to a Date
        gas_price = as.numeric(value),
        month = month(period),
        year  = year(period),
        region = duoarea
      ) |>
      select(gas_price, month, year) |>
      arrange(year, month)
  }
}
