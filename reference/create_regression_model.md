# Create regression model to predict bus ridership

This is a main function of the TRiP tool. It uses agency-level ridership
data and external census data to create a fixed effects regression model
that can help predict bus ridership. This function uses a backwards
stepwise method to create a model with only coefficients that have a
p-value less than 0.1.

## Usage

``` r
create_regression_model(
  data_xlsx,
  acs_data,
  gas_data,
  variables,
  fare_df = NULL
)
```

## Arguments

- data_xlsx:

  Agency-provided excel data as an r object of class `df`

- acs_data:

  Organized ACS data pulled from the census API using a few functions
  like
  [`pull_acs_data()`](https://kamrynmansfield.github.io/TRiP/reference/pull_acs_data.md)
  and
  [`create_final_acs_data()`](https://kamrynmansfield.github.io/TRiP/reference/create_final_acs_data.md)

- gas_data:

  Internal data storing historical gas prices obtained from [U.S. Energy
  Information
  Administration](https://www.eia.gov/dnav/pet/pet_pri_gnd_dcus_nus_m.htm)

- variables:

  A vector of variable names that will be tried in the regression model.

- fare_df:

  An optional data frame that describes the dates of the fare changes.
  Default is NULL.

## Value

Results of the regression model. An R oject of class "fixest"
