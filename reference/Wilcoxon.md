# Two-Sample Wilcoxon Test

Performs two-sample wilcoxon test on samples. In addition, an estimation
and a confidence interval for the location shift will be calculated.

## Super classes

[`LearnNonparam::PermuTest`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.md)
-\>
[`LearnNonparam::TwoSampleTest`](https://qddyy.github.io/LearnNonparam/reference/TwoSampleTest.md)
-\>
[`LearnNonparam::TwoSampleLocationTest`](https://qddyy.github.io/LearnNonparam/reference/TwoSampleLocationTest.md)
-\> `Wilcoxon`

## Active bindings

- `correct`:

  a logical indicating whether to apply continuity correction.

## Methods

### Public methods

- [`Wilcoxon$new()`](#method-Wilcoxon-new)

Inherited methods

- [`LearnNonparam::PermuTest$plot()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-plot)
- [`LearnNonparam::PermuTest$print()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-print)
- [`LearnNonparam::PermuTest$test()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-test)

------------------------------------------------------------------------

### Method `new()`

Create a new `Wilcoxon` object.

#### Usage

    Wilcoxon$new(
      type = c("permu", "asymp"),
      alternative = c("two_sided", "less", "greater"),
      null_value = 0,
      conf_level = 0.95,
      n_permu = 10000,
      correct = TRUE
    )

#### Arguments

- `type`:

  a character string specifying the way to calculate the p-value.

- `alternative`:

  a character string specifying the alternative hypothesis.

- `null_value`:

  a number indicating the true value of the location shift.

- `conf_level`:

  a number between zero and one indicating the confidence level to use.

- `n_permu`:

  an integer indicating number of permutations for the permutation
  distribution. If set to `0`, all permutations will be used.

- `correct`:

  a logical indicating whether to apply continuity correction in the
  normal approximation for the p-value.

#### Returns

A `Wilcoxon` object.

## Examples

``` r
pmt(
    "twosample.wilcoxon",
    alternative = "greater", n_permu = 0
)$test(Table2.1.1)$print()
#> 
#>       Two-Sample Wilcoxon Test 
#> 
#> scoring: rank    type: permu(35)    method: default
#> statistic = 21, p-value = 0.05714286
#> alternative hypothesis: true location shift is greater than 0
#> estimate: 16
#> 95% confidence interval: (-Inf, Inf)

pmt(
    "twosample.wilcoxon",
    alternative = "less", n_permu = 0
)$test(Table2.6.1)$print()
#> 
#>       Two-Sample Wilcoxon Test 
#> 
#> scoring: rank    type: permu(70)    method: default
#> statistic = 14, p-value = 0.1714286
#> alternative hypothesis: true location shift is less than 0
#> estimate: -0.35
#> 95% confidence interval: (-1.2, 0.5)

pmt(
    "twosample.wilcoxon", conf_level = 0.90
)$test(Table2.6.2)$conf_int
#> [1] 13.84 47.96
```
