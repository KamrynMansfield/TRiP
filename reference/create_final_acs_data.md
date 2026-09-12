# Prepare acs data for regression model

Aggregates tract-level ACS estimates up to the route level using the
buffer overlap weights from
[`create_intersecting_tract_percentages()`](https://kamrynmansfield.github.io/TRiP/reference/create_intersecting_tract_percentages.md),
then converts the annual series into a continuous monthly series between
`start_month` and `end_month`. Annual observations are anchored to
December of their year; months in between are linearly interpolated, and
months beyond the last ACS year are linearly extrapolated using the most
recent year-over-year change.

## Usage

``` r
create_final_acs_data(
  combined_acs_data,
  intersecting_tracts,
  start_month,
  end_month
)
```

## Arguments

- combined_acs_data:

  output from the
  [`combine_acs_data()`](https://kamrynmansfield.github.io/TRiP/reference/combine_acs_data.md)
  function

- intersecting_tracts:

  output from the
  [`create_intersecting_tract_percentages()`](https://kamrynmansfield.github.io/TRiP/reference/create_intersecting_tract_percentages.md)
  function

- start_month:

  the year and month (as a string separated by "-") that the final
  output will start with. Example: "2021-01"

- end_month:

  the year and month (as a string separated by "-") that the final
  output will end with. Example: "2025-12"

## Value

A data frame with one row per route per month between `start_month` and
`end_month`, a `date` column, and one column per ACS variable. This is
the `acs_data` argument expected by
[`create_regression_model()`](https://kamrynmansfield.github.io/TRiP/reference/create_regression_model.md),
`create_regression_model_forced()`, and
[`forecast_ridership()`](https://kamrynmansfield.github.io/TRiP/reference/forecast_ridership.md).

## Details

Work-from-home (`perc_wfh`) is treated specially: rather than
interpolating it smoothly it is carried forward as a step function,
because the sharp pandemic-era shift in telecommuting is better
represented as a level change than as a gradual trend.

## Warning

If the tract geometry covers more years than the ACS data does, the
tract data is filtered down to the ACS years and a warning is emitted.

## Examples

``` r
if (FALSE) { # \dontrun{
acs_final <- create_final_acs_data(
  combined_acs_data   = organized_acs,
  intersecting_tracts = tract_buffer_data,
  start_month         = "2021-01",
  end_month           = "2025-12"
)
} # }
```
