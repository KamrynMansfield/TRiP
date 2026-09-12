# Build the modeling data frame

Joins agency ridership data to route-level ACS data and gas prices,
converts monthly UPT into an average-weekday figure, adds the time-trend
and seasonality terms, applies an adult base fare series, and
log-transforms every continuous variable so the estimated coefficients
can be read as elasticities.

## Usage

``` r
make_model_data_frame(data_xlsx, acs_data, gas_data, fare_df = NULL)
```

## Arguments

- data_xlsx:

  The agency's uploaded ridership data as a data frame. Must contain
  `route_id`, `month`, `year`, `upt`, and `vrm`; may optionally contain
  `brt` and any number of extra numeric columns, which are picked up
  automatically and log-transformed.

- acs_data:

  Route-level monthly ACS data, as returned by
  [`create_final_acs_data()`](https://kamrynmansfield.github.io/TRiP/reference/create_final_acs_data.md).
  Must contain `route_id`, `date`, and the ACS variable columns.

- gas_data:

  Monthly gas price data with `month`, `year`, and `gas_price` columns.
  The app passes the internal `gas` object, sourced from the [U.S.
  Energy Information
  Administration](https://www.eia.gov/dnav/pet/pet_pri_gnd_dcus_nus_m.htm).

- fare_df:

  Optional data frame describing adult base fare changes, with
  `change_date`, `prev_fare`, and `new_fare` columns (built by the fare
  table on tab 2). If `NULL`, fare is held at a constant 1 so that
  `log_fare` is 0 and the variable drops out of the model.

## Value

A data frame with one row per route-month, containing the log-
transformed model variables (`log_upt_avg`, `log_vrm`, `log_gas_price`,
the `log_perc_*` ACS variables, `log_fare`), the untransformed helpers
(`year`, `month` as a factor, `year_cent`, `weekdays_in_month`, `time`),
and a `brt` logical column.

## Variables deliberately left untransformed

`workers_16_over`, `median_earnings`, and `emp_pop_ratio` are not
logged. `year`, `year_cent`, `month`, and `brt` are also left as-is
because they enter the model as a trend, a factor, and an indicator
respectively.
