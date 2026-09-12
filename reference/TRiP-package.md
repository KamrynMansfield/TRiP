# TRiP: Transit Ridership Prediction Tool

A Shiny application that forecasts bus ridership for small- to
medium-sized transit agencies. TRiP combines historical route-level
ridership with service levels, demographic and economic indicators, and
telecommuting rates to fit a fixed-effects regression model and project
ridership forward.

## Main workflow

The app proceeds through the following stages, each backed by functions
in this package:

1.  Load and screen agency ridership data

2.  Attach GTFS-derived service characteristics

3.  Join ACS demographics and gas price series

4.  Fit the regression model

5.  Forecast and visualize future ridership

## Getting started

Launch the application with
[`run_app()`](https://kamrynmansfield.github.io/TRiP/reference/run_app.md).

## See also

Useful links:

- <https://github.com/KamrynMansfield/TRiP>

- <https://kamrynmansfield.github.io/TRiP/>

- Report bugs at <https://github.com/KamrynMansfield/TRiP/issues>

## Author

**Maintainer**: Candace Brakewood <cvoulgaris@gsd.harvard.edu>
([ORCID](https://orcid.org/0000-0003-2769-7808)) \[contributor\]

Authors:

- Candace Brakewood <cvoulgaris@gsd.harvard.edu>
  ([ORCID](https://orcid.org/0000-0003-2769-7808)) \[contributor\]

- Kamryn Mansfield <kmansfi4@vols.utk.edu>
  ([ORCID](https://orcid.org/0009-0003-5466-0821)) \[contributor\]

- Mtthew Davis \[contributor\]
