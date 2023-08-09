
# LearnNonparam

[![R-CMD-check](https://github.com/qddyy/LearnNonparam/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/qddyy/LearnNonparam/actions/workflows/R-CMD-check.yaml)

This package implements the tests in chapters 1-5 of [Higgins
(2003)](#references).

It uses [R6](https://cran.r-project.org/package=R6) for clean OO-design
and [arrangements](https://cran.r-project.org/package=arrangements) for
fast generation of permutations.

## Installation

You can install the development version of LearnNonparam from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("qddyy/LearnNonparam")
```

## Usage

``` r
library(LearnNonparam)
```

- Create a test (for example, a `Wilcoxon` object)

  ``` r
  t <- Wilcoxon$new(type = "permu", n_permu = 10000)
  ```

  or you can use `pmt` (**p**er**m**utation **t**est) function
  (*Recommended*):

  ``` r
  t <- pmt("twosample.wilcoxon", type = "permu", n_permu = 10000)
  ```

- feed it the data (a data frame, a list, or some numeric vectors)

  ``` r
  t$feed(Table2.6.2)
  ```

- check the results

  ``` r
  t # or t$print()
  #> 
  #>   Two Sample Wilcoxon Test
  #> 
  #> statistic = 35, p-value = 0.0023
  #> alternative hypothesis:
  #>   true value of the parameter in the null hypothesis is not equal to 0 
  #> estimate: 30.045
  #> 95 percent confidence interval: 11.57 50.76

  t$plot(bins = 12)
  ```

  <img src="man/figures/README-unnamed-chunk-7-1.svg" width="100%" />

- modify some attributes and see how the results change

  ``` r
  t$type <- "approx"

  print(t$p_value)
  #> [1] 0.008239019
  ```

There is also support for chaining method calls, which means that you
can do things like

``` r
pmt("twosample.wilcoxon", type = "permu", n_permu = 10000)$feed(Table2.6.2)$print()$plot()
```

## Help

Just use `?...` syntax.

If you want to know all available methods and attributes, you can start
by exploring `?PermuTest` (all tests’ **base class**).

If you want to know all available tests, see `pmts()`.

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Higgins2003" class="csl-entry">

Higgins, James J. 2003. *An Introduction to Modern Nonparametric
Statistics*. Florence, KY: Brooks/Cole.

</div>

</div>
