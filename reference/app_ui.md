# Application User Interface

Builds the top-level UI. Not intended to be called directly; use
[`run_app()`](https://kamrynmansfield.github.io/TRiP/reference/run_app.md)
instead.

## Usage

``` r
app_ui()
```

## Value

A
[`shiny::tagList()`](https://rstudio.github.io/htmltools/reference/tagList.html)
containing the full UI definition.

## Details

The panel IDs are `"pan_1"` through `"pan_8"`, matching the eight
workflow steps listed in the file header.
[`app_server()`](https://kamrynmansfield.github.io/TRiP/reference/app_server.md)
moves the user between them with
`bslib::nav_select("main_nav", "pan_n")` once each step's prerequisites
are satisfied.

Several regions are rendered as `uiOutput()` placeholders rather than
static inputs, so that they only appear once the previous step has
succeeded. These include `screening_button_placeholder`,
`rider_data_next_placeholder`, `api_key_placeholder`,
`api_button_placeholder`, `acs_button_placeholder`,
`forced_coef_placeholder`, `brt_date`, and `brt_routes`.

## See also

[`app_server()`](https://kamrynmansfield.github.io/TRiP/reference/app_server.md)
for the corresponding server logic.
