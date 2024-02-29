
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

A few examples in the book can be found
[here](https://qddyy.github.io/LearnNonparam/articles/examples).

## Installation

``` r
# install.packages("pak")
pak::pkg_install("qddyy/LearnNonparam")
```

## Basic Usage

- Construct a test object

  - from some R6 class directly

  ``` r
  t <- Wilcoxon$new(alternative = "two_sided", type = "permu", n_permu = 1e6)
  ```

  - using the `pmt` (**p**er**m**utation **t**est) function
    (*recommended*)

  ``` r
  t <- pmt("twosample.wilcoxon", alternative = "two_sided", type = "permu", n_permu = 1e6)
  ```

- Provide it with samples

  ``` r
  t$test(rnorm(20, 1), rnorm(20, 0))
  ```

- Check the results

  ``` r
  t$statistic
  #> [1] 494
  t$p_value
  #> [1] 0.022888

  t$print()
  #> 
  #>       Two-Sample Wilcoxon Test 
  #> 
  #> scoring: rank    type: permu(1e+06)    method: default
  #> statistic = 494, p-value = 0.022888
  #> alternative hypothesis: true location shift is not equal to 0
  #> estimate: 0.9104679
  #> 95% confidence interval: (0.1180643, 1.572125)

  t$plot(style = "ggplot2", binwidth = 1)
  #> Loading required namespace: ggplot2
  ```

  <img src="man/figures/README-results-1.svg" width="100%" />

- Modify some active bindings and see how the results change

  ``` r
  t$type <- "asymp"

  t$print()
  #> 
  #>       Two-Sample Wilcoxon Test 
  #> 
  #> scoring: rank    type: asymp    method: default
  #> statistic = 494, p-value = 0.02390315
  #> alternative hypothesis: true location shift is not equal to 0
  #> estimate: 0.9104679
  #> 95% confidence interval: (0.1180643, 1.572125)
  ```

See `pmts()` for tests implemented in this package:

``` r
pmts()
```

<div class="kable-table">

| key                   | class              | test                                               |
|:----------------------|:-------------------|:---------------------------------------------------|
| onesample.quantile    | Quantile           | Quantile Test                                      |
| onesample.cdf         | CDF                | Inference on Cumulative Distribution Function      |
| twosample.difference  | Difference         | Two-Sample Test Based on Mean or Median            |
| twosample.wilcoxon    | Wilcoxon           | Two-Sample Wilcoxon Test                           |
| twosample.scoresum    | ScoreSum           | Two-Sample Test Based on Sum of Scores             |
| twosample.ansari      | AnsariBradley      | Ansari-Bradley Test                                |
| twosample.siegel      | SiegelTukey        | Siegel-Tukey Test                                  |
| twosample.rmd         | RatioMeanDeviance  | Ratio Mean Deviance Test                           |
| twosample.ks          | KolmogorovSmirnov  | Two-Sample Kolmogorov-Smirnov Test                 |
| ksample.f             | KSampleF           | K-Sample Test Based on F Statistic                 |
| ksample.kw            | KruskalWallis      | Kruskal-Wallis Test                                |
| ksample.jt            | JonckheereTerpstra | Jonckheere-Terpstra Test                           |
| multicomp.studentized | Studentized        | Multiple Comparison Based on Studentized Statistic |
| paired.sign           | Sign               | Two-Sample Sign Test                               |
| paired.difference     | PairedDifference   | Paired Comparison Based on Differences             |
| rcbd.f                | RCBDF              | Test for RCBD Based on F Statistic                 |
| rcbd.friedman         | Friedman           | Friedman Test                                      |
| rcbd.page             | Page               | Page Test                                          |
| association.corr      | Correlation        | Test for Association Between Paired Samples        |
| table.chisq           | ChiSquare          | Chi-Square Test on Contingency Table               |

</div>

## Experimental Features

The `define_pmt` function allows users to define new permutation tests.
Take Cramér-von Mises test as an example:

``` r
t <- define_pmt(
    # this is a two-sample permutation test
    inherit = "twosample",
    # provide a function to calculate the test statistic
    statistic = function(x, y) {
        F_n <- ecdf(x)
        G_n <- ecdf(y)
        sum(c(F_n(x) - G_n(x), F_n(y) - G_n(y))^2)
    },
    # reject the null hypothesis when the test statistic is large
    rejection = "r",
    scoring = "none", n_permu = 1e4,
    name = "Cramér-von Mises Test",
    alternative = "samples are from different distributions"
)

t$test(rnorm(20, 1), rnorm(20, 0))$print()
#> 
#>       Cramér-von Mises Test 
#> 
#> scoring: none    type: permu(10000)    method: default
#> statistic = 2.36, p-value = 0.0253
#> alternative hypothesis: samples are from different distributions
```

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Higgins2003" class="csl-entry">

Higgins, James J. 2003. *An Introduction to Modern Nonparametric
Statistics*. Florence, KY: Brooks/Cole.

</div>

</div>
