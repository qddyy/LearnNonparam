# Quantile Test

Performs quantile test on a single sample. In addition, an estimation
and a confidence interval for the desired quantile will be calculated.

## Super classes

[`LearnNonparam::PermuTest`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.md)
-\>
[`LearnNonparam::OneSampleTest`](https://qddyy.github.io/LearnNonparam/reference/OneSampleTest.md)
-\> `Quantile`

## Active bindings

- `prob`:

  The probability associated with the quantile.

- `correct`:

  a logical indicating whether to apply continuity correction.

## Methods

### Public methods

- [`Quantile$new()`](#method-Quantile-new)

Inherited methods

- [`LearnNonparam::PermuTest$print()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-print)
- [`LearnNonparam::PermuTest$test()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-test)
- [`LearnNonparam::OneSampleTest$plot()`](https://qddyy.github.io/LearnNonparam/reference/OneSampleTest.html#method-plot)

------------------------------------------------------------------------

### Method `new()`

Create a new `Quantile` object.

#### Usage

    Quantile$new(
      type = c("asymp", "exact"),
      alternative = c("two_sided", "less", "greater"),
      null_value = 0,
      conf_level = 0.95,
      prob = 0.5,
      correct = TRUE
    )

#### Arguments

- `type`:

  a character string specifying the way to calculate the p-value.

- `alternative`:

  a character string specifying the alternative hypothesis.

- `null_value`:

  a number indicating the hypothesized value of the quantile.

- `conf_level`:

  a number between zero and one indicating the confidence level to use.

- `prob`:

  a number between zero and one indicating the probability associated
  with the quantile.

- `correct`:

  a logical indicating whether to apply continuity correction in the
  normal approximation for the p-value.

#### Returns

A `Quantile` object.

## Examples

``` r
pmt(
    "onesample.quantile", prob = 0.5,
    null_value = 75, alternative = "greater",
    type = "asymp", correct = FALSE
)$test(Table1.1.1)$print()
#> 
#>       Quantile Test 
#> 
#> scoring: none    type: asymp    method: default
#> statistic = 26, p-value = 0.02888979
#> alternative hypothesis: true 0.5 quantile is greater than 75
#> estimate: 75.35
#> 95% confidence interval: (75, 77.1)

pmt(
    "onesample.quantile",
    prob = 0.25, conf_level = 0.90
)$test(Table1.2.1)$conf_int
#> [1] 11 29
```
