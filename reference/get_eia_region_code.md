# Get PADD code for specific region

Uses the county data frame to look up the PADD code. This is used to get
the proper gas data from the API. See
[here](https://www.eia.gov/todayinenergy/detail.php?id=4890) for more
details on PADD regions.

## Usage

``` r
get_eia_region_code(counties_df)
```

## Arguments

- counties_df:

  The dataframe created from the function
  [`find_overlapping_counties()`](https://kamrynmansfield.github.io/TRiP/reference/find_overlapping_counties.md)

## Value

The 3-character long PADD code for the EIA region

## Examples

``` r
if (FALSE) { # \dontrun{
padd_code <- get_eia_region_code(counties_df)
} # }
```
