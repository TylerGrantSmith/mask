
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mask

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/TylerGrantSmith/mask.svg?branch=master)](https://travis-ci.org/TylerGrantSmith/mask)
<!-- badges: end -->

The goal of mask is to …

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("TylerGrantSmith/mask")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(mask)
```

    ## Loading required package: rlang

``` r
m <- tidylog_mask()

m(mtcars %>% select(mpg) %>% filter(mpg < 15))
```

    ## select: dropped 10 variables (cyl, disp, hp, drat, wt, …)

    ## filter: removed 27 rows (84%), 5 rows remaining

    ##    mpg
    ## 1 14.3
    ## 2 10.4
    ## 3 10.4
    ## 4 14.7
    ## 5 13.3
