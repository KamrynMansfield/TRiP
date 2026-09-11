# Plat forecasted UPT

Plat forecasted UPT

## Usage

``` r
plot_forecast(forecast_df, route = "all_routes", scale = "average")
```

## Arguments

- forecast_df:

  The data frame with forecasted transit trips created from the
  forecast_ridership() function

- route:

  the name of the route you want to see. It must exist in the
  forecast_df

- scale:

  either "average" or "total" to specify whether you want the y axis to
  have average weekday UPT or total weekday UPT for the month.
