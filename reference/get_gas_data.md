# Get monthly U.S. retail gasoline prices from the EIA API

See [EIA Open Data](https://www.eia.gov/opendata/) page for more info.

## Usage

``` r
get_gas_data(
  start_month,
  start_year,
  end_month,
  end_year,
  region = "NUS",
  product = "EPM0"
)
```

## Arguments

- start_month:

  integer, 1-12

- start_year:

  integer, e.g. 2020

- end_month:

  integer, 1-12

- end_year:

  integer, e.g. 2024

- region:

  EIA duoarea code, e.g. "NUS" (U.S. avg), "R10"-"R50" (PADDs), or a
  state-level code. Defaults to U.S. average.

- product:

  EIA product code: "EPMR" = regular grade, "EPMM" = midgrade, "EPMP" =
  premium, "EPM0" = all grades. Defaults to all_grades.

## Value

tibble with columns: gas_price, month, year, region
