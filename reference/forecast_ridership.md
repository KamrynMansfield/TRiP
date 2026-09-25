# Forecast route-level ridership under Low, Medium, and High scenarios

Projects average weekday ridership forward through December of the year
following the reference month, for every route and all three scenarios.
The forecast is built by growing an observed reference month by a
product of factors: one per elasticity variable, one for the linear time
trend, one for the quadratic trend, one for monthly seasonality, and one
for any BRT conversion.

## Usage

``` r
forecast_ridership(
  coefs,
  data_xlsx,
  acs_data,
  gas_data,
  scenario_inputs_df,
  start_year = NULL,
  start_month = NULL,
  fare_df = NULL,
  brt_df = NULL,
  five_year = FALSE
)
```

## Arguments

- coefs:

  A named numeric vector of model coefficients, typically the output of
  the `final_coefs()` reactive (model coefficients with any user-forced
  literature values substituted in).

- data_xlsx:

  The agency's uploaded ridership data as a data frame.

- acs_data:

  Route-level monthly ACS data from
  [`create_final_acs_data()`](https://kamrynmansfield.github.io/TRiP/reference/create_final_acs_data.md).

- gas_data:

  Internal data storing historical gas prices obtained from [U.S. Energy
  Information
  Administration](https://www.eia.gov/dnav/pet/pet_pri_gnd_dcus_nus_m.htm)

- scenario_inputs_df:

  The saved scenario table, with `Variable`, `Low.Estimate`,
  `Mid.Estimate`, and `High.Estimate` columns. Passed to
  `organize_scenario_df()`.

- start_year:

  Year to anchor the forecast to. If `NULL` (the default) the most
  recent year in the data is used.

- start_month:

  Month to anchor the forecast to. If `NULL` (the default) the last
  observed month of `start_year` is used.

- fare_df:

  Optional data frame of adult base fare changes.

- brt_df:

  Optional data frame of planned BRT conversions, with `change_date_brt`
  and `routes_brt` columns, as built by the BRT table on tab 5.

- five_year:

  Option ability to forecast for the next 5 years instead of the next
  year. When `FALSE` (the default), it will only run a 1-year forecast.

## Value

A data frame combining observed history and forecast, with columns
`route_id`, `year`, `month`, `avg_daily_upt`, `tot_weekday_upt`,
`forecast` (logical), `scenario`, and `date`. Includes a synthetic
`"all_routes"` route that sums every route, and duplicates the reference
month across all three scenarios so the forecast lines connect cleanly
to the observed line when plotted.

## Argument mismatches worth checking

Same two issues flagged in `create_regression_model_forced()`:
[`app_server()`](https://kamrynmansfield.github.io/TRiP/reference/app_server.md)
calls this with `gas_data = gas` while the formal here is `gas_data` (an
"unused argument" error), and the call to
[`make_model_data_frame()`](https://kamrynmansfield.github.io/TRiP/reference/make_model_data_frame.md)
below passes five arguments to a four-argument function by adding
`brt_df`. Flagged rather than fixed.

## BRT coefficient naming

The BRT coefficient is looked up as `"brt"` here, but because `brt` is a
logical column the model reports it as `"brtTRUE"` (which is the name
[`check_coefficients()`](https://kamrynmansfield.github.io/TRiP/reference/check_coefficients.md)
uses). If the BRT factor ever comes back as exactly 1 when it shouldn't,
this naming mismatch is the first place to look.
