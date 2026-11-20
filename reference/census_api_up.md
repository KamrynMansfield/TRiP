# Check if census API is working

Check if census API is working

## Usage

``` r
census_api_up(timeout_sec = 5)
```

## Arguments

- timeout_sec:

  The number of seconds until it times out and returns FALSE

## Value

TRUE if the census API is working and FALSE if it is not working

## Examples

``` r
census_api_up()
#> [1] TRUE
```
