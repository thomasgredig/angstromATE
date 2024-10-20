# angstromATE

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/angstromATE)](https://CRAN.R-project.org/package=angstromATE)
<!-- badges: end -->

This R package loads Angstrom Engineering Thermal Evaporator Log Files and extracts a condensed version. 


## Installation

You can install the development version of angstromATE from [GitHub](https://github.com/thomasgredig/angstromATE) or with:

``` r
install.packages("angstromATE")
```


## Usage

Use the `ATE.import()` function to load a log file.

``` r
library(angstromATE)
fileName = ATE.sampleFiles('csv') # sample file
df <- ATE.import(fileName)
head(df)
```

