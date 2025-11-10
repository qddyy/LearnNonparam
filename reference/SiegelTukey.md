# Siegel-Tukey Test

Performs Siegel-Tukey test on samples.

## Super classes

[`LearnNonparam::PermuTest`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.md)
-\>
[`LearnNonparam::TwoSampleTest`](https://qddyy.github.io/LearnNonparam/reference/TwoSampleTest.md)
-\>
[`LearnNonparam::TwoSampleLocationTest`](https://qddyy.github.io/LearnNonparam/reference/TwoSampleLocationTest.md)
-\>
[`LearnNonparam::Wilcoxon`](https://qddyy.github.io/LearnNonparam/reference/Wilcoxon.md)
-\> `SiegelTukey`

## Methods

### Public methods

- [`SiegelTukey$new()`](#method-SiegelTukey-new)

Inherited methods

- [`LearnNonparam::PermuTest$plot()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-plot)
- [`LearnNonparam::PermuTest$print()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-print)
- [`LearnNonparam::PermuTest$test()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-test)

------------------------------------------------------------------------

### Method `new()`

Create a new `SiegelTukey` object.

#### Usage

    SiegelTukey$new(
      type = c("permu", "asymp"),
      alternative = c("two_sided", "less", "greater"),
      n_permu = 10000,
      correct = TRUE
    )

#### Arguments

- `type`:

  a character string specifying the way to calculate the p-value.

- `alternative`:

  a character string specifying the alternative hypothesis.

- `n_permu`:

  an integer indicating number of permutations for the permutation
  distribution. If set to `0`, all permutations will be used.

- `correct`:

  a logical indicating whether to apply continuity correction in the
  normal approximation for the p-value.

#### Returns

A `SiegelTukey` object.

## Examples

``` r
pmt(
    "twosample.siegel",
    alternative = "greater", n_permu = 0
)$test(Table2.8.1)$print()
#> 
#>       Siegel-Tukey Test 
#> 
#> scoring: Siegel-Tukey rank    type: permu(252)    method: default
#> statistic = 24, p-value = 0.2738095
#> alternative hypothesis: true ratio of scales is greater than 1
```
