
# rWishart

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/rWishart)](https://cran.r-project.org/package=rWishart)
[![](http://cranlogs.r-pkg.org/badges/grand-total/rWishart)](https://cran.r-project.org/package=rWishart)
[![Travis-CI Build
Status](https://travis-ci.org/BenBarnard/rWishart.svg?branch=master)](https://travis-ci.org/BenBarnard/rWishart)
[![codecov](https://codecov.io/gh/BenBarnard/rWishart/branch/master/graph/badge.svg)](https://codecov.io/gh/BenBarnard/rWishart)

## Overview

rWishart is a wishart distribution suite. In creating other packages we
found it useful to separate out our wishart generation functions and
maintain them somewhere else. This package works well with slidR,
EqualCov and covEst.

In developing covEst we found it useful to have it play nice with the
“tidyverse.” At this point we have not thought of all the uses and
combinations with these packages so if you think of something not
currently implemented please file a minimal reproducible example on
github.

## Installation

You can install the latest development version from github with

``` r
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
devtools::install_github("benbarnard/rWishart")
```

If you encounter a clear bug, please file a minimal reproducible example
on github.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(rWishart)

rWishart(n = 1, df = 2, Sigma = diag(1, 10), covariance = TRUE)
```
