# Prepare acs data for regression model

Prepare acs data for regression model

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

a data frame with the needed variables to go into the regression model
