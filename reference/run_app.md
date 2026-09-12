# Run the Shiny Application

Launches the TRiP forecasting application in the user's default browser.
The app walks a transit agency through eight tabs: screening, ridership
data upload, GTFS upload, model creation, model review, forecasting
inputs, visualization, and export.

## Usage

``` r
run_app(...)
```

## Arguments

- ...:

  Arguments passed to
  [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html),
  such as `options` (for example
  `options = list(launch.browser = TRUE, port = 8080)`).

## Value

A Shiny app object. Called for its side effect of launching the app.

## See also

[`app_ui()`](https://kamrynmansfield.github.io/TRiP/reference/app_ui.md)
and
[`app_server()`](https://kamrynmansfield.github.io/TRiP/reference/app_server.md)
for the UI and server definitions.

## Examples

``` r
if (FALSE) { # \dontrun{
run_app()
} # }
```
