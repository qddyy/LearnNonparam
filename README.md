
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

Examples in the book can be found
[here](https://qddyy.github.io/LearnNonparam/articles/examples).

## Installation

``` r
# install.packages("pak")
pak::pkg_install("qddyy/LearnNonparam")
```

## Usage

- Create a test object (for example, a `Wilcoxon` object)

  ``` r
  t <- Wilcoxon$new(alternative = "two_sided", type = "permu", n_permu = 1e6)
  ```

  or you can use `pmt` (**p**er**m**utation **t**est) function
  (*Recommended*):

  ``` r
  t <- pmt("twosample.wilcoxon", alternative = "two_sided", type = "permu", n_permu = 1e6)
  ```

- Test some data

  ``` r
  t$test(rnorm(20, 1), rnorm(20, 0))
  ```

- Check the results

  ``` r
  t$p_value
  #> [1] 0.008967

  t$print(digits = 2)
  #> 
  #>       Two Sample Wilcoxon Test 
  #> 
  #> scoring: rank    type: permu(1e+06)    method: default
  #> statistic = 497, p-value = 0.009
  #> alternative hypothesis: true location shift is not equal to 0
  #> estimate: 0.8
  #> 95% confidence interval: (0.13, 1.4)

  t$plot(style = "ggplot2", binwidth = 1)
  #> Loading required namespace: ggplot2
  ```

  <img src="man/figures/README-results-1.svg" width="100%" />

- Modify some attributes and see how the results change

  ``` r
  t$type <- "asymp"

  t$p_value
  #> [1] 0.01929238
  ```

All available tests:

``` r
pmts()
```

<div class="kable-table">

| key                  | class              | test                                                 |
|:---------------------|:-------------------|:-----------------------------------------------------|
| onesample.quantile   | Quantile           | Quantile Test                                        |
| onesample.cdf        | CDF                | Cumulative Distribution Function                     |
| twosample.difference | Difference         | Two Sample Test Based on Mean or Median              |
| twosample.wilcoxon   | Wilcoxon           | Two Sample Wilcoxon Test                             |
| twosample.scoresum   | ScoreSum           | Score Sum Test                                       |
| twosample.ansari     | AnsariBradley      | Ansari-Bradley Test                                  |
| twosample.siegel     | SiegelTukey        | Siegel-Tukey Test                                    |
| twosample.rmd        | RatioMeanDeviance  | Ratio Mean Deviance Test                             |
| twosample.ks         | KolmogorovSmirnov  | Two Sample Kolmogorov-Smirnov Test                   |
| ksample.anova        | ANOVA              | K Sample Test Based on F Statistic                   |
| ksample.kw           | KruskalWallis      | Kruskal-Wallis Test                                  |
| ksample.jt           | JonckheereTerpstra | Jonckheere-Terpstra Test                             |
| multicomp.t          | MultiCompT         | Multiple Comparison Based on t Statistic             |
| multicomp.tukey      | TukeyHSD           | Tukey’s HSD                                          |
| paired.sign          | Sign               | Sign Test                                            |
| paired.difference    | PairedDifference   | Paired Comparison Based on Differences               |
| rcbd.anova           | RCBDANOVA          | ANOVA for Randomized Complete Block Design           |
| rcbd.friedman        | Friedman           | Friedman Test                                        |
| rcbd.page            | Page               | Page Test                                            |
| association.corr     | Correlation        | Two Sample Test Based on Correlation Coefficient     |
| table.chisq          | ChiSquare          | Contingency Table Test Based on Chi-square Statistic |

</div>

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Higgins2003" class="csl-entry">

Higgins, James J. 2003. *An Introduction to Modern Nonparametric
Statistics*. Florence, KY: Brooks/Cole.

</div>

</div>
