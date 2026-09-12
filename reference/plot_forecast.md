# Plot forecasted UPT

Draws observed and forecasted ridership for one route as a time series,
with one colored line per scenario. The observed history appears as the
"Observed" series and the three projected scenarios branch off from the
reference month.

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
  forecast_df. Defaults to `"all_routes"`, the synthetic route that sums
  ridership across the whole system.

- scale:

  either "average" or "total" to specify whether you want the y axis to
  have average weekday UPT or total weekday UPT for the month.

## Value

A `ggplot` object.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_forecast(forecast_df)                       # system total
plot_forecast(forecast_df, route = "14")         # a single route
plot_forecast(forecast_df, scale = "total")      # monthly totals instead
} # }
```
