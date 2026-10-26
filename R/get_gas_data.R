#' Get monthly U.S. retail gasoline prices from the EIA API
#'
#' See [EIA Open Data](https://www.eia.gov/opendata/) page for more info.
#'
#' @param start_month integer, 1-12
#' @param start_year integer, e.g. 2020
#' @param end_month integer, 1-12
#' @param end_year integer, e.g. 2024
#' @param api_key character, your EIA API key
#' @param region EIA duoarea code, e.g. "NUS" (U.S. avg) or "R10"-"R50" (PADDs),
#'   Defaults to U.S. average.
#' @param product EIA product code: "EPMR" = regular grade, "EPMM" = midgrade,
#'   "EPMP" = premium, "EPM0" = all grades. Defaults to all_grades.
#'
#' @return tibble with columns: gas_price, month, year
get_gas_data <- function(start_month,
                         start_year,
                         end_month,
                         end_year,
                         api_key,
                         region = "NUS",
                         product = "EPM0") {

  # EIA expects YYYY-MM formatted start/end dates
  start_date <- sprintf("%04d-%02d", start_year, start_month)
  end_date   <- sprintf("%04d-%02d", end_year, end_month)

  base_url <- "https://api.eia.gov/v2/petroleum/pri/gnd/data/"

  query_params <- list(
    api_key              = api_key,
    frequency            = "monthly",
    "data[0]"            = "value",
    "facets[duoarea][]"  = region,
    "facets[product][]"  = product,
    start                = start_date,
    end                  = end_date,
    "sort[0][column]"    = "period",
    "sort[0][direction]" = "asc",
    offset               = 0,
    length               = 5000
  )

  resp <- httr::GET(base_url, query = query_params)
  httr::warn_for_status(resp, task = "retrieve data from EIA API")

  parsed <- jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    simplifyVector = TRUE
  )

  df <- parsed$response$data

  if (is.null(df) || nrow(df) == 0) {
    warning("No data returned from EIA API for the specified parameters.")
    return(NULL)
  }

  out <- tibble::tibble(
    gas_price = as.numeric(df$value),
    month     = as.numeric(substr(df$period, 6, 7)),
    year      = as.numeric(substr(df$period, 1, 4))
  )

  out <- out[order(out$year, out$month), ]
  tibble::as_tibble(out)
}
