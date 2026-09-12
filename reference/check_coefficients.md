# Check model coefficient reasonableness

Builds a formatted coefficient table for the chosen model and colors any
row whose sign contradicts theory or prior research: red for an error (a
sign that should never occur, such as a negative VRM elasticity, which
would mean running more service reduces ridership) and yellow for a
warning (a sign worth a second look). Rows with no expectation on file
are left uncolored.

## Usage

``` r
check_coefficients(model, extra_vars)
```

## Arguments

- model:

  A model created to forecast bus ridership in the TRiP app, typically
  the `fixest` object stored in `selected_model()`.

- extra_vars:

  A named character vector of any additional variables the agency
  uploaded, in `c("Display Name" = "log_variable")` form, as produced by
  the `addnl_vars()` reactive in
  [`app_server()`](https://kamrynmansfield.github.io/TRiP/reference/app_server.md).
  These are appended to the lookup so custom columns get readable labels
  instead of `NA`.

## Value

A `gt` table of coefficients that is colored to point out unexpected
signs. The helper `sign_check` column is hidden before rendering, and
the `Message` column only appears when at least one variable triggered a
flag.

## Examples

``` r
if (FALSE) { # \dontrun{
check_coefficients(selected_model(), addnl_vars())
} # }
```
