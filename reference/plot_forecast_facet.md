# Plot forecasted UPT for all routes in a facet wrap

Plot forecasted UPT for all routes in a facet wrap

## Usage

``` r
plot_forecast_facet(forecast_df, scale = "average")
```

## Arguments

- forecast_df:

  The data frame with forecasted transit trips created from the
  forecast_ridership() function

- scale:

  either "average" or "total" to specify whether you want the y axis to
  have average weekday UPT or total weekday UPT for the month.
