# TRiP

This is where you will find documentation to the R code behind the
**T**ransit **Ri**dership **P**rediction (TRiP) Tool.

## Introduction

The goal of the TRiP Tool is to help small- to medium- sized transit
agencies to predict their future ridership.

## Access

Click
[here](https://019f7142-d057-c4b5-68a3-3421b30aab9e.share.connect.posit.cloud/)
to access the Current TRiP Tool

Click [here](https://github.com/KamrynMansfield/TRiP) to access the
github repository for the TRiP Tool Shiny App.

The R code below will install the development version of the TRiP R
package from [GitHub](https://github.com/). This is not recommended as
the r package mainly exists to document the Shiny App, but you are
welcome to install it in order to use the functions created for the
Shiny App.

``` r

# install.packages("pak")
pak::pak("KamrynMansfield/TRiP")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r

library(TRiP)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r

summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

![](reference/figures/README-pressure-1.png)

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
