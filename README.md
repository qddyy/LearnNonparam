
# LearnNonparam <img src="man/figures/logo.svg" alt="logo" width="14%" align="right"/>

[![GPL
license](https://img.shields.io/github/license/qddyy/LearnNonparam)](https://cran.r-project.org/web/licenses/GPL-2)
[![GitHub R package
version](https://img.shields.io/github/r-package/v/qddyy/LearnNonparam)](https://github.com/qddyy/LearnNonparam)
[![Code
size](https://img.shields.io/github/languages/code-size/qddyy/LearnNonparam.svg)](https://github.com/qddyy/LearnNonparam)
[![CodeFactor](https://www.codefactor.io/repository/github/qddyy/LearnNonparam/badge)](https://www.codefactor.io/repository/github/qddyy/LearnNonparam)
[![R CMD
check](https://github.com/qddyy/LearnNonparam/workflows/R-CMD-check/badge.svg)](https://github.com/qddyy/LearnNonparam/actions)

## Overview

This package implements some of the non-parametric tests in chapters 1-5
of [Higgins (2003)](#references).

It depends on [R6](https://CRAN.R-project.org/package=R6) for object
oriented design and [Rcpp](https://CRAN.R-project.org/package=Rcpp) for
integration of R and C++.

Examples in the book can be found
[here](https://qddyy.github.io/LearnNonparam/articles/examples).

## Installation

``` r
# install.packages("pak")
pak::pkg_install("qddyy/LearnNonparam")
```

## Usage

- Construct a test object

  - from some R6 class directly

  ``` r
  t <- Wilcoxon$new(alternative = "two_sided", type = "permu", n_permu = 1e6)
  ```

  - using `pmt` (**p**er**m**utation **t**est) function (*recommended*)

  ``` r
  t <- pmt("twosample.wilcoxon", alternative = "two_sided", type = "permu", n_permu = 1e6)
  ```

- Test some data

  ``` r
  t$test(rnorm(20, 1), rnorm(20, 0))
  ```

- Check the results

  ``` r
  t$statistic
  #> [1] 495
  t$p_value
  #> [1] 0.010332

  t$print(digits = 2)
  #> 
  #>       Two Sample Wilcoxon Test 
  #> 
  #> scoring: rank    type: permu(1e+06)    method: default
  #> statistic = 495, p-value = 0.01
  #> alternative hypothesis: true location shift is not equal to 0
  #> estimate: 0.99
  #> 95% confidence interval: (0.097, 1.6)

  t$plot(style = "ggplot2", binwidth = 1)
  #> Loading required namespace: ggplot2
  ```

  <img src="man/figures/README-results-1.svg" width="100%" />

- Modify some active bindings and see how the results change

  ``` r
  t$type <- "asymp"

  t$p_value
  #> [1] 0.02226991
  ```

Tests implemented in this package:

``` r
pmts()
```

<div class="kable-table">

| key                | class    | test                             |
|:-------------------|:---------|:---------------------------------|
| onesample.quantile | Quantile | Quantile Test                    |
| onesample.cdf      | CDF      | Cumulative Distribution Function |

</div>

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Higgins2003" class="csl-entry">

Higgins, James J. 2003. *An Introduction to Modern Nonparametric
Statistics*. Florence, KY: Brooks/Cole.

</div>

</div>
