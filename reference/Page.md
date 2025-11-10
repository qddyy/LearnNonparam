# Page Test

Performs Page test on samples collected in a randomized complete block
design.

## Super classes

[`LearnNonparam::PermuTest`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.md)
-\>
[`LearnNonparam::RCBDTest`](https://qddyy.github.io/LearnNonparam/reference/RCBDTest.md)
-\> `Page`

## Methods

### Public methods

- [`Page$new()`](#method-Page-new)

Inherited methods

- [`LearnNonparam::PermuTest$plot()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-plot)
- [`LearnNonparam::PermuTest$print()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-print)
- [`LearnNonparam::PermuTest$test()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-test)

------------------------------------------------------------------------

### Method `new()`

Create a new `Page` object.

#### Usage

    Page$new(
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

A `Page` object.

## Examples

``` r
t <- pmt(
    "rcbd.page", alternative = "less"
)$test(Table4.4.3)$print()
#> 
#>       Page Test 
#> 
#> scoring: rank    type: permu(10000)    method: default
#> statistic = 134, p-value = 0.0112 (± 0.002062583 at 95% confidence)
#> alternative hypothesis: less

t$type <- "asymp"
t
#> 
#>       Page Test 
#> 
#> scoring: rank    type: asymp    method: default
#> statistic = 134, p-value = 0.01182581
#> alternative hypothesis: less
```
