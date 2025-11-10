# Two-Sample Test Based on Mean or Median

Performs mean/median based two-sample test on samples.

## Super classes

[`LearnNonparam::PermuTest`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.md)
-\>
[`LearnNonparam::TwoSampleTest`](https://qddyy.github.io/LearnNonparam/reference/TwoSampleTest.md)
-\>
[`LearnNonparam::TwoSampleLocationTest`](https://qddyy.github.io/LearnNonparam/reference/TwoSampleLocationTest.md)
-\> `Difference`

## Methods

### Public methods

- [`Difference$new()`](#method-Difference-new)

Inherited methods

- [`LearnNonparam::PermuTest$plot()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-plot)
- [`LearnNonparam::PermuTest$print()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-print)
- [`LearnNonparam::PermuTest$test()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-test)

------------------------------------------------------------------------

### Method `new()`

Create a new `Difference` object.

#### Usage

    Difference$new(
      method = c("mean", "median"),
      alternative = c("two_sided", "less", "greater"),
      null_value = 0,
      n_permu = 10000
    )

#### Arguments

- `method`:

  a character string specifying whether to use the mean or the median.

- `alternative`:

  a character string specifying the alternative hypothesis.

- `null_value`:

  a number indicating the true value of the location shift.

- `n_permu`:

  an integer indicating number of permutations for the permutation
  distribution. If set to `0`, all permutations will be used.

#### Returns

A `Difference` object.

## Examples

``` r
pmt(
    "twosample.difference", method = "mean",
    alternative = "greater", n_permu = 0
)$test(Table2.1.1)$print()$plot(
    style = "graphic", breaks = seq(-20, 25, length.out = 9)
)
#> 
#>       Two-Sample Test Based on Mean or Median 
#> 
#> scoring: none    type: permu(35)    method: mean
#> statistic = 16.16667, p-value = 0.05714286
#> alternative hypothesis: greater


pmt(
    "twosample.difference", method = "mean",
    alternative = "greater", n_permu = 1000
)$test(Table2.3.1)$print()
#> 
#>       Two-Sample Test Based on Mean or Median 
#> 
#> scoring: none    type: permu(1000)    method: mean
#> statistic = 1.675, p-value = 0.196 (± 0.02460391 at 95% confidence)
#> alternative hypothesis: greater
```
