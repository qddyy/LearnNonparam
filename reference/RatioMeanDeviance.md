# Ratio Mean Deviance Test

Performs ratio mean deviance test on samples.

## Super classes

[`LearnNonparam::PermuTest`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.md)
-\>
[`LearnNonparam::TwoSampleTest`](https://qddyy.github.io/LearnNonparam/reference/TwoSampleTest.md)
-\> `RatioMeanDeviance`

## Methods

### Public methods

- [`RatioMeanDeviance$new()`](#method-RatioMeanDeviance-new)

Inherited methods

- [`LearnNonparam::PermuTest$plot()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-plot)
- [`LearnNonparam::PermuTest$print()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-print)
- [`LearnNonparam::PermuTest$test()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-test)

------------------------------------------------------------------------

### Method `new()`

Create a new `RatioMeanDeviance` object.

#### Usage

    RatioMeanDeviance$new(
      alternative = c("two_sided", "less", "greater"),
      n_permu = 10000
    )

#### Arguments

- `alternative`:

  a character string specifying the alternative hypothesis.

- `n_permu`:

  an integer indicating number of permutations for the permutation
  distribution. If set to `0`, all permutations will be used.

#### Returns

A `RatioMeanDeviance` object.

## Examples

``` r
pmt(
    "twosample.rmd",
    alternative = "greater", n_permu = 0
)$test(Table2.8.1)$print()
#> 
#>       Ratio Mean Deviance Test 
#> 
#> scoring: none    type: permu(252)    method: default
#> statistic = 4.666667, p-value = 0.07936508
#> alternative hypothesis: true ratio of scales is greater than 1
```
