# One-Way Test for Equal Means in RCBD

Performs F statistic based one-way test on samples collected in a
randomized complete block design.

## Super classes

[`LearnNonparam::PermuTest`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.md)
-\>
[`LearnNonparam::RCBDTest`](https://qddyy.github.io/LearnNonparam/reference/RCBDTest.md)
-\> `RCBDOneWay`

## Methods

### Public methods

- [`RCBDOneWay$new()`](#method-RCBDOneWay-new)

Inherited methods

- [`LearnNonparam::PermuTest$plot()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-plot)
- [`LearnNonparam::PermuTest$print()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-print)
- [`LearnNonparam::PermuTest$test()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-test)

------------------------------------------------------------------------

### Method `new()`

Create a new `RCBDOneWay` object.

#### Usage

    RCBDOneWay$new(type = c("permu", "asymp"), n_permu = 10000)

#### Arguments

- `type`:

  a character string specifying the way to calculate the p-value.

- `n_permu`:

  an integer indicating number of permutations for the permutation
  distribution. If set to `0`, all permutations will be used.

#### Returns

A `RCBDOneWay` object.

## Examples

``` r
t <- pmt(
    "rcbd.oneway", n_permu = 5000
)$test(Table4.4.3)$print()
#> 
#>       One-Way Test for Equal Means in RCBD 
#> 
#> scoring: none    type: permu(5000)    method: default
#> statistic = 110241.8, p-value = 0.0582 (± 0.006489391 at 95% confidence)

t$type <- "asymp"
t
#> 
#>       One-Way Test for Equal Means in RCBD 
#> 
#> scoring: none    type: asymp    method: default
#> statistic = 3.121093, p-value = 0.057516
```
