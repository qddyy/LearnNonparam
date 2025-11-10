# Paired Comparison Based on Differences

Performs differences based paired comparison on samples.

## Super classes

[`LearnNonparam::PermuTest`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.md)
-\>
[`LearnNonparam::TwoSampleTest`](https://qddyy.github.io/LearnNonparam/reference/TwoSampleTest.md)
-\>
[`LearnNonparam::TwoSamplePairedTest`](https://qddyy.github.io/LearnNonparam/reference/TwoSamplePairedTest.md)
-\> `PairedDifference`

## Active bindings

- `correct`:

  Whether to apply continuity correction when `scoring` is set to
  `"rank"`.

## Methods

### Public methods

- [`PairedDifference$new()`](#method-PairedDifference-new)

Inherited methods

- [`LearnNonparam::PermuTest$plot()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-plot)
- [`LearnNonparam::PermuTest$print()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-print)
- [`LearnNonparam::PermuTest$test()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-test)

------------------------------------------------------------------------

### Method `new()`

Create a new `PairedDifference` object.

#### Usage

    PairedDifference$new(
      type = c("permu", "asymp"),
      method = c("with_zeros", "without_zeros"),
      scoring = c("none", "rank", "vw", "expon"),
      alternative = c("two_sided", "less", "greater"),
      null_value = 0,
      n_permu = 10000,
      correct = TRUE
    )

#### Arguments

- `type`:

  a character string specifying the way to calculate the p-value.

- `method`:

  a character string specifying the method of ranking data in computing
  adjusted signed scores for tied data, must be one of `"with_zeros"`
  (default) or `"without_zeros"`.

- `scoring`:

  a character string specifying the scoring system.

- `alternative`:

  a character string specifying the alternative hypothesis.

- `null_value`:

  a number indicating the true value of the location shift.

- `n_permu`:

  an integer indicating number of permutations for the permutation
  distribution. If set to `0`, all permutations will be used.

- `correct`:

  a logical indicating whether to apply continuity correction in the
  normal approximation for the p-value when `scoring` is set to
  `"rank"`.

#### Returns

A `PairedDifference` object.

## Examples

``` r
pmt(
    "paired.difference",
    alternative = "greater", scoring = "none", n_permu = 0
)$test(Table4.1.1)$print()
#> 
#>       Paired Comparison Based on Differences 
#> 
#> scoring: none    type: permu(32)    method: with_zeros
#> statistic = 1080, p-value = 0.03125
#> alternative hypothesis: greater

pmt(
    "paired.difference", n_permu = 0
)$test(Table4.1.3)$print()
#> 
#>       Paired Comparison Based on Differences 
#> 
#> scoring: none    type: permu(131072)    method: with_zeros
#> statistic = 183, p-value = 1.525879e-05
#> alternative hypothesis: two_sided

t <- pmt(
    "paired.difference", scoring = "rank",
    alternative = "greater", n_permu = 0
)$test(Table4.1.1)$print()
#> 
#>       Paired Comparison Based on Differences 
#> 
#> scoring: rank    type: permu(32)    method: with_zeros
#> statistic = 15, p-value = 0.03125
#> alternative hypothesis: greater

t$type <- "asymp"
t
#> 
#>       Paired Comparison Based on Differences 
#> 
#> scoring: rank    type: asymp    method: with_zeros
#> statistic = 15, p-value = 0.02952911
#> alternative hypothesis: greater
```
