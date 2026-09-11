# Use to county data frame to get needed acs tables

Use to county data frame to get needed acs tables

## Usage

``` r
pull_acs_data(county_sf, years = 2024)
```

## Arguments

- county_sf:

  the county sf object that was output by
  [`find_overlapping_counties()`](https://kamrynmansfield.github.io/TRiP/reference/find_overlapping_counties.md)

- years:

  the years that you want the tables for

## Value

a nested list with acs data for each year and each table we want
(tables: "B08006","B08201", "DP05","S0802","S2301")
