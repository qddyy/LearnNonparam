# Syntactic Sugar for Object Construction

Construct test objects in a unified way.

## Usage

``` r
pmt(key, ...)

pmts(
  which = c("all", "onesample", "twosample", "distribution", "association", "paired",
    "ksample", "multcomp", "rcbd", "table")
)

define_pmt(
  method = c("twosample", "distribution", "association", "paired", "ksample", "rcbd",
    "table"),
  statistic,
  rejection = c("<>", "<", ">"),
  scoring = "none",
  n_permu = 10000,
  name = "User-Defined Permutation Test",
  alternative = NULL,
  quickr = FALSE,
  depends = character(),
  plugins = character(),
  includes = character()
)
```

## Arguments

- key:

  a character string specifying the test. Check `pmts()` for valid keys.

- ...:

  extra parameters passed to the constructor.

- which:

  a character string specifying the desired tests.

- method:

  a character string specifying the permutation scheme.

- statistic:

  definition of the test statistic. See details.

- rejection:

  a character string specifying the rejection region relative to the
  test statistic.

- scoring:

  one of: - a character string in `c("none", "rank", "vw", "expon")`
  specifying the scoring system - a function that takes a numeric vector
  and returns an equal-length score vector

- n_permu:

  an integer indicating number of permutations for the permutation
  distribution. If set to `0`, all permutations will be used.

- name, alternative:

  character strings specifying the name of the test and the alternative
  hypothesis, used for printing purposes only.

- quickr:

  a logical indicating whether to use
  [`quickr::quick()`](https://rdrr.io/pkg/quickr/man/quick.html) to
  accelerate `statistic`. See details.

- depends, plugins, includes:

  passed to
  [`Rcpp::cppFunction()`](https://rdrr.io/pkg/Rcpp/man/cppFunction.html).

## Value

a test object corresponding to the specified key.

a data frame containing keys and corresponding tests implemented in this
package.

a test object based on the specified statistic.

## Details

The test statistic can be defined using either R or Rcpp, with the
`statistic` parameter specified as:

- **R**: a closure returning one of

  - a double (the test statistic).

  - a closure returning a double.

- **Rcpp**: a character string defining a captureless lambda (since
  C++11) returning another lambda that captures by value, accepts
  parameters of the same type, and returns a double.

This design aims to pre-calculate potential constants that remain
invariant during permutation.

When using Rcpp, the parameters for different `method` are listed as
follows. Note that the names can be customized, and the types can be
replaced with `auto` (thanks to the support for generic lambdas in
C++14). See examples.

|                  |                                             |                                              |
|------------------|---------------------------------------------|----------------------------------------------|
| `method`         | Parameter 1                                 | Parameter 2                                  |
| `"twosample"`    | `const NumericVector& sample_1`             | `const NumericVector& sample_2`              |
| `"distribution"` | `const NumericVector& cumulative_prob_1`    | `const NumericVector& cumulative_prob_2`     |
| `"association"`  | `const NumericVector& sample_1`             | `const NumericVector& sample_2`              |
| `"paired"`       | `const NumericVector& sample_1`             | `const NumericVector& sample_2`              |
| `"ksample"`      | `const NumericVector& combined_sample`      | `const IntegerVector& one_based_group_index` |
| `"rcbd"`         | `const NumericMatrix& block_as_column_data` |                                              |
| `"table"`        | `const IntegerMatrix& contingency_table`    |                                              |

When using R, `statistic` and the parameters should be the R equivalents
of the above. If no constants exist during permutation, `statistic` may
simply be an R closure returning a double.

If `quickr = TRUE` and `statistic` returns a double, it will be compiled
to Fortran via
[`quickr::quick()`](https://rdrr.io/pkg/quickr/man/quick.html) with
[`base::declare()`](https://rdrr.io/r/base/declare.html) calls for all
arguments inserted automatically. Otherwise, `statistic` will be
compiled using
[`compiler::cmpfun()`](https://rdrr.io/r/compiler/compile.html).

## Note

To improve performance when calling R closures from C++, this package
repeatedly evaluates the closure's body in an environment whose
enclosing environment is the closure's own, with its formal arguments
pre-assigned to the data. This imposes the following restrictions on the
closure returning the test statistic when `statistic` is written in R:

- Do not re-assign its formal arguments or any pre-computed symbols in
  its environment.

- Do not use default arguments or variadic arguments.

It's also worth noting that the data is permuted in-place. Therefore,
modifications to the data within `statistic` may lead to incorrect
results. It is recommended to avoid modifying the data when using R and
pass const references as in the table above when using Rcpp.

## Examples

``` r
pmt("twosample.wilcoxon")
#> <Wilcoxon>
#>   Inherits from: <TwoSampleLocationTest>
#>   Public:
#>     alternative: active binding
#>     conf_int: active binding
#>     conf_level: active binding
#>     correct: active binding
#>     data: active binding
#>     estimate: active binding
#>     initialize: function (type = c("permu", "asymp"), alternative = c("two_sided", 
#>     method: active binding
#>     n_permu: active binding
#>     null_value: active binding
#>     p_value: active binding
#>     plot: function (style = c("graphics", "ggplot2"), ...) 
#>     print: function () 
#>     scoring: active binding
#>     statistic: active binding
#>     test: function (...) 
#>     type: active binding
#>   Private:
#>     .alternative: two_sided
#>     .autoplot: function (...) 
#>     .calculate: function () 
#>     .calculate_extra: function () 
#>     .calculate_n_permu: function () 
#>     .calculate_p: function () 
#>     .calculate_p_permu: function () 
#>     .calculate_score: function () 
#>     .calculate_side: function () 
#>     .calculate_statistic: function () 
#>     .compile: function () 
#>     .conf_int: NULL
#>     .conf_level: 0.95
#>     .correct: TRUE
#>     .data: NULL
#>     .define: function () 
#>     .estimate: NULL
#>     .link: +
#>     .method: default
#>     .n_permu: 10000
#>     .name: Two-Sample Wilcoxon Test
#>     .null_value: 0
#>     .on_alternative_change: function () 
#>     .on_conf_level_change: function () 
#>     .on_method_change: function () 
#>     .on_n_permu_change: function () 
#>     .on_null_value_change: function () 
#>     .on_scoring_change: function () 
#>     .on_type_change: function () 
#>     .p_value: NULL
#>     .param_name: location shift
#>     .plot: function (...) 
#>     .preprocess: function () 
#>     .print: function () 
#>     .raw_data: NULL
#>     .scoring: rank
#>     .side: NULL
#>     .statistic: NULL
#>     .statistic_func: NULL
#>     .type: permu

pmts("ksample")
#>              key              class                         test
#> 1 ksample.oneway             OneWay One-Way Test for Equal Means
#> 2     ksample.kw      KruskalWallis          Kruskal-Wallis Test
#> 3     ksample.jt JonckheereTerpstra     Jonckheere-Terpstra Test

x <- rnorm(5)
y <- rnorm(5, 1)

t <- define_pmt(
    method = "twosample", rejection = "<",
    scoring = base::rank, # equivalent to "rank"
    statistic = function(x, y) sum(x)
)$test(x, y)$print()
#> 
#>       User-Defined Permutation Test 
#> 
#> scoring: custom    type: permu(10000)    method: twosample
#> statistic = 28, p-value = 0.5785 (± 0.009678288 at 95% confidence)

t$scoring <- function(x) qnorm(rank(x) / (length(x) + 1)) # equivalent to "vw"
t$print()
#> 
#>       User-Defined Permutation Test 
#> 
#> scoring: custom    type: permu(10000)    method: twosample
#> statistic = 0.1141853, p-value = 0.5487 (± 0.009753225 at 95% confidence)

t$n_permu <- 0
t$print()
#> 
#>       User-Defined Permutation Test 
#> 
#> scoring: custom    type: permu(252)    method: twosample
#> statistic = 0.1141853, p-value = 0.5436508

# \donttest{
r <- define_pmt(
    method = "twosample", n_permu = 1e5,
    statistic = function(x, y) {
        m <- length(x)
        n <- length(y)
        function(x, y) sum(x) / m - sum(y) / n
    }
)

quickr <- define_pmt(
    method = "twosample", n_permu = 1e5, quickr = TRUE,
    statistic = function(x, y) sum(x) / length(x) - sum(y) / length(y)
)

rcpp <- define_pmt(
    method = "twosample", n_permu = 1e5,
    statistic = "[](const auto& x, const auto& y) {
        auto m = x.length();
        auto n = y.length();
        return [=](const auto& x, const auto& y) {
            return sum(x) / m - sum(y) / n;
        };
    }"
)

# equivalent
# rcpp <- define_pmt(
#     method = "twosample", n_permu = 1e5,
#     statistic = "[](const NumericVector& x, const NumericVector& y) {
#         R_xlen_t m = x.length();
#         R_xlen_t n = y.length();
#         return [m, n](const NumericVector& x, const NumericVector& y) -> double {
#             return sum(x) / m - sum(y) / n;
#         };
#     }"
# )

set.seed(0)
r$test(x, y)$print()
#> 
#>       User-Defined Permutation Test 
#> 
#> scoring: none    type: permu(1e+05)    method: twosample
#> statistic = 0.002255881, p-value = 1 (± 0 at 95% confidence)
set.seed(0)
quickr$test(x, y)$print()
#> Loading required namespace: quickr
#> 
#>       User-Defined Permutation Test 
#> 
#> scoring: none    type: permu(1e+05)    method: twosample
#> statistic = 0.002255881, p-value = 1 (± 0 at 95% confidence)
set.seed(0)
rcpp$test(x, y)$print()
#> 
#>       User-Defined Permutation Test 
#> 
#> scoring: none    type: permu(1e+05)    method: twosample
#> statistic = 0.002255881, p-value = 1 (± 0 at 95% confidence)

options(LearnNonparam.pmt_progress = FALSE)
system.time(r$test(x, y))
#>    user  system elapsed 
#>   0.112   0.000   0.112 
system.time(quickr$test(x, y))
#>    user  system elapsed 
#>   0.034   0.000   0.034 
system.time(rcpp$test(x, y))
#>    user  system elapsed 
#>   0.007   0.000   0.007 
# }
```
