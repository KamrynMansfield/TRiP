# Plot forecasted UPT for all routes in a facet wrap

Same chart as
[`plot_forecast()`](https://kamrynmansfield.github.io/TRiP/reference/plot_forecast.md)
but faceted, with one small panel per route. Each panel gets free y and
x scales, since ridership levels differ by orders of magnitude between a
trunk route and a coverage route and a shared scale would flatten the
smaller ones.

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

## Value

A `ggplot` object with one facet per `route_id`.

## See also

[`plot_forecast()`](https://kamrynmansfield.github.io/TRiP/reference/plot_forecast.md)
for the single-route version used in the app.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_forecast_facet(forecast_df)
plot_forecast_facet(forecast_df, scale = "total")
} # }
```
