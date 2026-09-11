# Get census tract geometry for given counties

This function uses the tirgis package to retrieve the census tract
boundaries.

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
using `st_make_valid`
