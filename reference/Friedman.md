# Friedman Test

Performs Friedman test on samples collected in a randomized complete
block design.

## Super classes

[`LearnNonparam::PermuTest`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.md)
-\>
[`LearnNonparam::RCBDTest`](https://qddyy.github.io/LearnNonparam/reference/RCBDTest.md)
-\> `Friedman`

## Methods

### Public methods

- [`Friedman$new()`](#method-Friedman-new)

Inherited methods

- [`LearnNonparam::PermuTest$plot()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-plot)
- [`LearnNonparam::PermuTest$print()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-print)
- [`LearnNonparam::PermuTest$test()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-test)

------------------------------------------------------------------------

### Method `new()`

Create a new `Friedman` object.

#### Usage

    Friedman$new(type = c("permu", "asymp"), n_permu = 10000)

#### Arguments

- `type`:

  a character string specifying the way to calculate the p-value.

- `n_permu`:

  an integer indicating number of permutations for the permutation
  distribution. If set to `0`, all permutations will be used.

#### Returns

A `Friedman` object.

## Examples

``` r
t <- pmt(
    "rcbd.friedman", n_permu = 0
)$test(Table4.5.3)$print()
#> 
#>       Friedman Test 
#> 
#> scoring: rank    type: permu(576)    method: default
#> statistic = 28.33333, p-value = 0.04166667

t$type <- "asymp"
t
#> 
#>       Friedman Test 
#> 
#> scoring: rank    type: asymp    method: default
#> statistic = 7.5, p-value = 0.05755845
```
