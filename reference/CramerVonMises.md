# Two-Sample Cramer-Von Mises Test

Performs two-sample Cramer-Von Mises test on samples.

## Super classes

[`LearnNonparam::PermuTest`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.md)
-\>
[`LearnNonparam::TwoSampleTest`](https://qddyy.github.io/LearnNonparam/reference/TwoSampleTest.md)
-\>
[`LearnNonparam::TwoSampleDistributionTest`](https://qddyy.github.io/LearnNonparam/reference/TwoSampleDistributionTest.md)
-\> `CramerVonMises`

## Methods

### Public methods

- [`CramerVonMises$new()`](#method-CramerVonMises-new)

Inherited methods

- [`LearnNonparam::PermuTest$plot()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-plot)
- [`LearnNonparam::PermuTest$print()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-print)
- [`LearnNonparam::PermuTest$test()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-test)

------------------------------------------------------------------------

### Method `new()`

Create a new `CramerVonMises` object.

#### Usage

    CramerVonMises$new(
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

A `CramerVonMises` object.

## Examples

``` r
pmt(
    "distribution.cvm", n_permu = 0
)$test(Table2.8.1)$print()
#> 
#>       Two-Sample Cramer-Von Mises Test 
#> 
#> scoring: none    type: permu(252)    method: default
#> statistic = 0.36, p-value = 0.8730159
#> alternative hypothesis: two_sided
```
