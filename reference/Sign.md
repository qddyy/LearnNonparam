# Two-Sample Sign Test

Performs two-sample sign test on samples.

## Super classes

[`LearnNonparam::PermuTest`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.md)
-\>
[`LearnNonparam::TwoSampleTest`](https://qddyy.github.io/LearnNonparam/reference/TwoSampleTest.md)
-\>
[`LearnNonparam::TwoSamplePairedTest`](https://qddyy.github.io/LearnNonparam/reference/TwoSamplePairedTest.md)
-\> `Sign`

## Active bindings

- `correct`:

  a logical indicating whether to apply continuity correction.

## Methods

### Public methods

- [`Sign$new()`](#method-Sign-new)

Inherited methods

- [`LearnNonparam::PermuTest$plot()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-plot)
- [`LearnNonparam::PermuTest$print()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-print)
- [`LearnNonparam::PermuTest$test()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-test)

------------------------------------------------------------------------

### Method `new()`

Create a new `Sign` object.

#### Usage

    Sign$new(
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

A `Sign` object.

## Examples

``` r
t <- pmt(
    "paired.sign",
    alternative = "greater", n_permu = 0
)$test(
    rep(c(+1, -1), c(12, 5)), rep(0, 17)
)$print()
#> 
#>       Two-Sample Sign Test 
#> 
#> scoring: none    type: permu(131072)    method: default
#> statistic = 12, p-value = 0.07173157
#> alternative hypothesis: greater

t$type <- "asymp"
t
#> 
#>       Two-Sample Sign Test 
#> 
#> scoring: none    type: asymp    method: default
#> statistic = 12, p-value = 0.07280505
#> alternative hypothesis: greater
```
