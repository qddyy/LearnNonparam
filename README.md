
# LearnNonparam

[![R-CMD-check](https://github.com/qddyy/LearnNonparam/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/qddyy/LearnNonparam/actions/workflows/R-CMD-check.yaml)

This package implements most of the tests in chapters 1-5 of [Higgins
(2003)](#references).

It uses [R6](https://cran.r-project.org/package=R6) for clean OO-design
and [arrangements](https://cran.r-project.org/package=arrangements) for
fast generation of permutations, as well as
[ggplot2](https://cran.r-project.org/package=ggplot2) to draw pretty
graphs.

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

- feed it the data (a data frame, a list, or some numeric vectors)

  ``` r
  t$feed(rnorm(20, mean = 1), rnorm(20, mean = 0))
  ```

- check the results

  ``` r
  t$p_value
  #> [1] 0.001341

  t$print()
  #> 
  #>       Two Sample Wilcoxon Test 
  #> 
  #> type: permu    method: default    
  #> statistic = 519, p-value = 0.001341, 
  #> alternative hypothesis: greater 
  #> estimate: 1.076735 
  #> 95 percent confidence interval: 0.3611423 1.7557380

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

## Help

Just use `?...` syntax.

If you want to know all available methods and attributes, you can start
by exploring `?PermuTest` (all tests’ **base class**).

If you want to know all available tests, see `pmts()`.

``` r
pmts()
```

<div class="kable-table">

| key                | class              | test                                                 |
|:-------------------|:-------------------|:-----------------------------------------------------|
| onesample.quantile | Quantile           | Quantile Test                                        |
| onesample.cdf      | CDF                | Cumulative Distribution Function                     |
| twosample.mean     | MeanDiff           | Two Sample Test Based on Mean                        |
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
| paired.comparison  | PairedComparison   | Paired Comparison                                    |
| paired.sign        | Sign               | Sign Test                                            |
| paired.signedscore | SignedScore        | Signed Score Test                                    |
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
