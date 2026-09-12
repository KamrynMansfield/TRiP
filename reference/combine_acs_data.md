# Organize Pulled ACS Data

Takes the per-year list returned by
[`pull_acs_data()`](https://kamrynmansfield.github.io/TRiP/reference/pull_acs_data.md),
stacks it into a single long data frame, renames the cryptic ACS
variable IDs to readable names, pivots to one column per variable, and
converts the raw commute and vehicle counts into percentages of the
relevant denominator.

## Usage

``` r
combine_acs_data(pulled_acs_list)
```

## Arguments

- pulled_acs_list:

  The nested list created by
  [`pull_acs_data()`](https://kamrynmansfield.github.io/TRiP/reference/pull_acs_data.md).
  Should contain only successful year elements; check that the `errors`
  element is absent before calling this.

## Value

An `sf` data frame with one row per tract per year and one column per
ACS variable, including the derived `perc_car`, `perc_taxicab`,
`perc_wfh`, and `perc_hshlds_noveh` columns. The raw count columns used
to build those percentages are dropped.

## Examples

``` r
if (FALSE) { # \dontrun{
acs_data_list <- pull_acs_data(county_sf, 2020:2024)
combined_acs  <- combine_acs_data(acs_data_list)
} # }
```
