# LearnNonparam

[![License](https://img.shields.io/cran/l/LearnNonparam?color=orange)](https://cran.r-project.org/web/licenses/GPL-2)
[![CRAN
status](https://www.r-pkg.org/badges/version/LearnNonparam)](https://cran.r-project.org/package=LearnNonparam)
[![Dependencies](https://tinyverse.netlify.app/badge/LearnNonparam)](https://cran.r-project.org/package=LearnNonparam)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/LearnNonparam)](https://r-pkg.org/pkg/LearnNonparam)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/LearnNonparam)](https://r-pkg.org/pkg/LearnNonparam)

## Overview

This R package implements several non-parametric tests in chapters 1-5
of [Higgins (2004)](#references), including tests for one sample, two
samples, k samples, paired comparisons, blocked designs, trends and
association. Built with [Rcpp](https://CRAN.R-project.org/package=Rcpp)
for efficiency and [R6](https://CRAN.R-project.org/package=R6) for
flexible, object-oriented design, it provides a unified framework for
performing or creating custom permutation tests.

## Installation

Install the stable version from
[CRAN](https://CRAN.R-project.org/package=LearnNonparam):

``` r
install.packages("LearnNonparam")
```

Install the development version from
[Github](https://github.com/qddyy/LearnNonparam):

``` r
# install.packages("remotes")
remotes::install_github("qddyy/LearnNonparam")
```

## Usage

``` r
library(LearnNonparam)
```

- Construct a test object

  - from some R6 class directly

  ``` r
  t <- Wilcoxon$new(n_permu = 1e6)
  ```

  - using the `pmt` (**p**er**m**utation **t**est) wrapper

  ``` r
  # recommended for a unified API
  t <- pmt("twosample.wilcoxon", n_permu = 1e6)
  ```

- Provide it with samples

  ``` r
  set.seed(-1)

  t$test(rnorm(10, 1), rnorm(10, 0))
  ```

  ![](reference/figures/README/test.svg)

- Check the results

  ``` r
  t$statistic
  ```

  ![](reference/figures/README/statistic.svg)

  ``` r
  t$p_value
  ```

  ![](reference/figures/README/p_value.svg)

  ``` r
  options(digits = 3)

  t$print()
  ```

  ![](reference/figures/README/print.svg)

  ``` r
  ggplot2::theme_set(ggplot2::theme_minimal())

  t$plot(style = "ggplot2", binwidth = 1) # or ggplot2::autoplot(t, binwidth = 1)
  ```

  ![](reference/figures/README/plot.svg)

  ![](./reference/figures/README/histogram.svg)

- Modify some settings and observe the change

  ``` r
  t$type <- "asymp"
  t$p_value
  ```

  ![](reference/figures/README/modify.svg)

See `pmts()` for tests implemented in this package.

``` r
pmts()
```

| key                  | class              | test                                               |
|:---------------------|:-------------------|:---------------------------------------------------|
| onesample.quantile   | Quantile           | Quantile Test                                      |
| onesample.cdf        | CDF                | Inference on Cumulative Distribution Function      |
| twosample.difference | Difference         | Two-Sample Test Based on Mean or Median            |
| twosample.wilcoxon   | Wilcoxon           | Two-Sample Wilcoxon Test                           |
| twosample.ansari     | AnsariBradley      | Ansari-Bradley Test                                |
| twosample.siegel     | SiegelTukey        | Siegel-Tukey Test                                  |
| twosample.rmd        | RatioMeanDeviance  | Ratio Mean Deviance Test                           |
| distribution.ks      | KolmogorovSmirnov  | Two-Sample Kolmogorov-Smirnov Test                 |
| distribution.kuiper  | Kuiper             | Two-Sample Kuiper Test                             |
| distribution.cvm     | CramerVonMises     | Two-Sample Cramer-Von Mises Test                   |
| distribution.ad      | AndersonDarling    | Two-Sample Anderson-Darling Test                   |
| association.corr     | Correlation        | Test for Association Between Paired Samples        |
| paired.sign          | Sign               | Two-Sample Sign Test                               |
| paired.difference    | PairedDifference   | Paired Comparison Based on Differences             |
| ksample.oneway       | OneWay             | One-Way Test for Equal Means                       |
| ksample.kw           | KruskalWallis      | Kruskal-Wallis Test                                |
| ksample.jt           | JonckheereTerpstra | Jonckheere-Terpstra Test                           |
| multcomp.studentized | Studentized        | Multiple Comparison Based on Studentized Statistic |
| rcbd.oneway          | RCBDOneWay         | One-Way Test for Equal Means in RCBD               |
| rcbd.friedman        | Friedman           | Friedman Test                                      |
| rcbd.page            | Page               | Page Test                                          |
| table.chisq          | ChiSquare          | Chi-Square Test on Contingency Table               |

## Extending

`define_pmt` allows users to define new permutation tests. Take the
two-sample Wilcoxon test as an example:

``` r
t_custom <- define_pmt(
    # this is a two-sample permutation test
    method = "twosample",
    statistic = function(x, y) {
        # (optional) pre-calculate certain constants that remain invariant during permutation
        m <- length(x)
        n <- length(y)
        # return a closure to calculate the test statistic
        function(x, y) sum(x) / m - sum(y) / n
    },
    # reject the null hypothesis when the test statistic is too large or too small
    rejection = "<>", n_permu = 1e5
)
```

![](reference/figures/README/define_r.svg)

For R \>= 4.4.0, the [quickr](https://CRAN.R-project.org/package=quickr)
package can be used to accelerate `statistic`. However, this results in
repeated crossings of the R-Fortran boundary and makes pre-calculation
of constants impossible.

``` r
t_quickr <- define_pmt(
    method = "twosample", rejection = "<>", n_permu = 1e5,
    statistic = function(x, y) {
        sum(x) / length(x) - sum(y) / length(y)
    },
    quickr = TRUE
)
```

![](reference/figures/README/define_quickr.svg)

In cases where both pre-calculation and computational efficiency are
required, the statistic can be written in C++. Leveraging Rcpp sugars
and C++14 features, only minor modifications are needed to make it
compatible with C++ syntax.

``` r
t_cpp <- define_pmt(
    method = "twosample", rejection = "<>", n_permu = 1e5,
    statistic = "[](const auto& x, const auto& y) {
        auto m = x.length();
        auto n = y.length();
        return [=](const auto& x, const auto& y) {
            return sum(x) / m - sum(y) / n;
        };
    }"
)
```

![](reference/figures/README/define_cpp.svg)

It’s easy to check that `t_custom`, `t_quickr` and `t_cpp` are
equivalent:

``` r
x <- rnorm(10, mean = 0)
y <- rnorm(10, mean = 5)
```

![](reference/figures/README/prepare_data.svg)

``` r
set.seed(0)
t_custom$test(x, y)$print()
```

![](reference/figures/README/t_custom_res.svg)

``` r
set.seed(0)
t_quickr$test(x, y)$print()
```

![](reference/figures/README/t_quickr_res.svg)

``` r
set.seed(0)
t_cpp$test(x, y)$print()
```

![](reference/figures/README/t_cpp_res.svg)

## Performance

[coin](https://CRAN.R-project.org/package=coin) is a commonly used R
package for performing permutation tests. Below is a benchmark:

``` r
library(coin)

data <- c(x, y)
group <- factor(c(rep("x", length(x)), rep("y", length(y))))

options(LearnNonparam.pmt_progress = FALSE)
benchmark <- microbenchmark::microbenchmark(
    pure_R = t_custom$test(x, y),
    quickr = t_quickr$test(x, y),
    Rcpp = t_cpp$test(x, y),
    coin = wilcox_test(data ~ group, distribution = approximate(nresample = 1e5, parallel = "no"))
)
```

![](reference/figures/README/benchmark.svg)

``` r
benchmark
```

![](reference/figures/README/benchmark_res.svg)

It can be seen that C++ brings significantly better performance than
pure R, which enables it to even surpass the coin package in its
no-parallelization setting. However, all tests in this package are
currently written in pure R with no plans for migration to C++ in the
future. This is because the primary goal of this package is not to
maximize performance but to offer a flexible framework for permutation
tests.

## References

Higgins, J. J. 2004. *An Introduction to Modern Nonparametric
Statistics*. Duxbury Advanced Series. Brooks/Cole.
