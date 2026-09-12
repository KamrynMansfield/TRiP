# Application Server Logic

Implements every reactive, observer, and output for the TRiP app. Not
called directly;
[`run_app()`](https://kamrynmansfield.github.io/TRiP/reference/run_app.md)
passes it to
[`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html).

## Usage

``` r
app_server(input, output, session)
```

## Arguments

- input, output, session:

  Internal Shiny parameters.

## Value

Called for side effects. Returns `NULL` invisibly.

## See also

[`app_ui()`](https://kamrynmansfield.github.io/TRiP/reference/app_ui.md)
for the interface these handlers are bound to.
