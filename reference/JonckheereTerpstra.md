# Jonckheere-Terpstra Test

Performs Jonckheere-Terpstra test on samples.

## Super classes

[`LearnNonparam::PermuTest`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.md)
-\>
[`LearnNonparam::KSampleTest`](https://qddyy.github.io/LearnNonparam/reference/KSampleTest.md)
-\> `JonckheereTerpstra`

## Methods

### Public methods

- [`JonckheereTerpstra$new()`](#method-JonckheereTerpstra-new)

Inherited methods

- [`LearnNonparam::PermuTest$plot()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-plot)
- [`LearnNonparam::PermuTest$print()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-print)
- [`LearnNonparam::PermuTest$test()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-test)

------------------------------------------------------------------------

### Method `new()`

Create a new `JonckheereTerpstra` object.

#### Usage

    JonckheereTerpstra$new(
      type = c("permu", "asymp"),
      alternative = c("two_sided", "less", "greater"),
      n_permu = 10000
    )

#### Arguments

- `type`:

  a character string specifying the way to calculate the p-value.

- `alternative`:

  a character string specifying the alternative hypothesis.

- `n_permu`:

  an integer indicating number of permutations for the permutation
  distribution. If set to `0`, all permutations will be used.

#### Returns

A `JonckheereTerpstra` object.

## Examples

``` r
t <- pmt(
    "ksample.jt", alternative = "greater"
)$test(Table3.4.1)$print()
#> 
#>       Jonckheere-Terpstra Test 
#> 
#> scoring: none    type: permu(10000)    method: default
#> statistic = 145, p-value = 0.0307 (± 0.003381009 at 95% confidence)
#> alternative hypothesis: greater

t$type <- "asymp"
t
#> 
#>       Jonckheere-Terpstra Test 
#> 
#> scoring: none    type: asymp    method: default
#> statistic = 145, p-value = 0.0285154
#> alternative hypothesis: greater
```
