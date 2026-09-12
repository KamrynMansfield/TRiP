# Use the county data frame to get needed acs tables

Downloads the fixed set of ACS 5-year variables the TRiP model relies
on, at the census tract level, for every county the routes pass through.
One API call is made per year so that each year can be matched to the
tract vintage of the same year.

## Usage

``` r
pull_acs_data(county_sf, years = 2024)
```

## Arguments

- county_sf:

  the county sf object that was output by
  [`find_overlapping_counties()`](https://kamrynmansfield.github.io/TRiP/reference/find_overlapping_counties.md)

- years:

  the years that you want the tables for. Defaults to 2024.

## Value

A named list with one element per year holding a long-format
`tidycensus` data frame (with geometry), plus a final `errors` element
listing any years that failed to download.
[`app_server()`](https://kamrynmansfield.github.io/TRiP/reference/app_server.md)
checks for a non-empty `errors` element and warns the user rather than
proceeding.

## Details

Requires a Census API key to be registered (the app collects one on tab
3 and installs it with
[`tidycensus::census_api_key()`](https://walker-data.com/tidycensus/reference/census_api_key.html)
before calling this).

The variables pulled come from five tables:

- B08006:

  Means of transportation to work (total workers, WFH, taxi, car,
  transit, bike, walk)

- B08201:

  Household size by vehicles available (total households, households
  with no vehicle)

- DP05:

  Demographic profile (total population)

- S0802:

  Commuting characteristics (poverty ratios, median earnings, share
  female, share renter-occupied)

- S2301:

  Employment status (labor force participation, employment/population
  ratio, unemployment)

## Examples

``` r
if (FALSE) { # \dontrun{
acs_data_list <- pull_acs_data(county_sf, 2020:2024)
} # }
```
