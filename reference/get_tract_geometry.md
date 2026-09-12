# Get census tract geometry for given counties

This function uses the tigris package to retrieve the census tract
boundaries for every requested year and stacks them into a single `sf`
object with a `year` column. Because tract boundaries are redrawn
between vintages, each year must be fetched separately and later joined
to ACS data of the same year.

## Usage

``` r
get_tract_geometry(state_fips, county_fips, years)
```

## Arguments

- state_fips:

  A state fips or vector of state fips

- county_fips:

  A county fips or vector of county fips

- years:

  A year or list of years

## Value

An sf object of the requested tract boundaries that have been validated
using `st_make_valid`, with an added `year` column. Returns `NULL` if
any requested year failed to download or came back empty.

## Details

Any failure (no internet, a Census.gov outage, an unreleased vintage, or
an empty result) causes the function to return `NULL` rather than
partial data.
[`app_server()`](https://kamrynmansfield.github.io/TRiP/reference/app_server.md)
checks for that `NULL` and shows the user a modal explaining the likely
causes.

## Examples

``` r
if (FALSE) { # \dontrun{
state_fips  <- unique(county_sf$STATEFP)
county_fips <- unique(county_sf$COUNTYFP)
tract_geom  <- get_tract_geometry(state_fips, county_fips, 2020:2024)
} # }
```
