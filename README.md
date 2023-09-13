
# LearnNonparam

[![GitHub R package
version](https://img.shields.io/github/r-package/v/qddyy/LearnNonparam)](https://github.com/qddyy/LearnNonparam)
[![MIT
License](https://img.shields.io/badge/license-MIT-blue.svg)](https://cran.r-project.org/web/licenses/MIT)
[![Code
size](https://img.shields.io/github/languages/code-size/qddyy/LearnNonparam.svg)](https://github.com/qddyy/LearnNonparam)
[![CodeFactor](https://www.codefactor.io/repository/github/qddyy/LearnNonparam/badge)](https://www.codefactor.io/repository/github/qddyy/LearnNonparam)
[![R CMD
check](https://github.com/qddyy/LearnNonparam/workflows/R-CMD-check/badge.svg)](https://github.com/qddyy/LearnNonparam/actions)

## Overview

This package implements most of the tests in chapters 1-5 of [Higgins
(2003)](#references).

It uses [R6](https://cran.r-project.org/package=R6) for clean OO-design
and [RcppAlgos](https://cran.r-project.org/package=RcppAlgos) for fast
generation of combinations/permutations, as well as
[ggplot2](https://cran.r-project.org/package=ggplot2) to draw pretty
graphs.

Examples in the book can be found
[here](https://qddyy.github.io/LearnNonparam/articles/examples).

## Installation

You can install the development version of this package with:

``` r
# install.packages("remotes")
remotes::install_github("qddyy/LearnNonparam")
```

## Usage

``` r
library(LearnNonparam)
```

- Create a test object (for example, a `Wilcoxon` object)

  ``` r
  t <- Wilcoxon$new(alternative = "greater", type = "permu", n_permu = 1000000)
  ```

  or you can use `pmt` (**p**er**m**utation **t**est) function
  (*Recommended*):

  ``` r
  t <- pmt("twosample.wilcoxon", alternative = "greater", type = "permu", n_permu = 1000000)
  ```

- feed it the data (a data frame, a list, or numeric vectors)

  ``` r
  t$feed(rnorm(20, mean = 1), rnorm(20, mean = 0))
  ```

- check the results

  ``` r
  t$p_value
  #> [1] 0.001336

  t$print()
  #> 
  #>       Two Sample Wilcoxon Test 
  #> 
  #> scoring: rank    type: permu(1000000)    method: default
  #> statistic = 519, p-value = 0.001336
  #> alternative hypothesis: greater
  #> estimate: 0.9461215
  #> 95 percent confidence interval: 0.3867042 1.6248357

  t$plot(binwidth = 1)
  ```

  <img src="man/figures/README-results-1.svg" width="100%" />

- modify some attributes and see how the results change

  ``` r
  t$type <- "approx"

  t$p_value
  #> [1] 0.00166809
  ```

There is also support for chaining method calls, which means that you
can do things like

``` r
t <- pmt(...)$feed(...)$print(...)$plot(...)
```

## Tips

- Use `options(pmt_seed = ...)` to initialize the random seed.

- Use `options(pmt_progress = FALSE)` to disable the progress bar.

  (By default, a progress bar will appear when calculating the
  permutation distribution if R is used interactively)

- Explore `?PermuTest` (all tests’ base class) for all available methods
  and attributes.

- Check `pmts()` for all available tests.

  ``` r
  pmts()
  ```

  <div class="kable-table">

  | key                | class              | test                                                 |
  |:-------------------|:-------------------|:-----------------------------------------------------|
  | onesample.quantile | Quantile           | Quantile Test                                        |
  | onesample.cdf      | CDF                | Cumulative Distribution Function                     |
  | twosample.mean     | Mean               | Two Sample Test Based on Mean                        |
  | twosample.wilcoxon | Wilcoxon           | Two Sample Wilcoxon Test                             |
  | twosample.scoresum | ScoreSum           | Score Sum Test                                       |
  | twosample.ansari   | AnsariBradley      | Ansari-Bradley Test                                  |
  | twosample.siegel   | SiegelTukey        | Siegel-Tukey Test                                    |
  | twosample.rmd      | RatioMeanDeviance  | Ratio Mean Deviance Test                             |
  | twosample.ks       | KolmogorovSmirnov  | Two Sample Kolmogorov-Smirnov Test                   |
  | ksample.anova      | ANOVA              | K Sample Test Based on F Statistic                   |
  | ksample.kw         | KruskalWallis      | Kruskal-Wallis Test                                  |
  | ksample.jt         | JonckheereTerpstra | Jonckheere-Terpstra Test                             |
  | multicomp.t        | MultiCompT         | Multiple Comparison Based on t Statistic             |
  | multicomp.tukey    | TukeyHSD           | Tukey’s HSD                                          |
  | paired.sign        | Sign               | Sign Test                                            |
  | paired.signeddiff  | SignedDiff         | Paired Comparison Based on Signed Differences        |
  | rcbd.anova         | RCBDANOVA          | ANOVA for Randomized Complete Block Design           |
  | rcbd.friedman      | Friedman           | Friedman Test                                        |
  | rcbd.page          | Page               | Page Test                                            |
  | association.corr   | Correlation        | Two Sample Test Based on Correlation Coefficient     |
  | table.chisq        | ChiSquare          | Contingency Table Test Based on Chi-square Statistic |

  </div>

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Higgins2003" class="csl-entry">

Higgins, James J. 2003. *An Introduction to Modern Nonparametric
Statistics*. Florence, KY: Brooks/Cole.

</div>

</div>
