# One-Way Test for Equal Means

Performs F statistic based one-way test on samples.

## Super classes

[`LearnNonparam::PermuTest`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.md)
-\>
[`LearnNonparam::KSampleTest`](https://qddyy.github.io/LearnNonparam/reference/KSampleTest.md)
-\> `OneWay`

## Methods

### Public methods

- [`OneWay$new()`](#method-OneWay-new)

Inherited methods

- [`LearnNonparam::PermuTest$plot()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-plot)
- [`LearnNonparam::PermuTest$print()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-print)
- [`LearnNonparam::PermuTest$test()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-test)

------------------------------------------------------------------------

### Method `new()`

Create a new `OneWay` object.

#### Usage

    OneWay$new(type = c("permu", "asymp"), n_permu = 10000)

#### Arguments

- `type`:

  a character string specifying the way to calculate the p-value.

- `n_permu`:

  an integer indicating number of permutations for the permutation
  distribution. If set to `0`, all permutations will be used.

#### Returns

A `OneWay` object.

## Examples

``` r
# \donttest{
t <- pmt(
    "ksample.oneway", n_permu = 0
)$test(Table3.1.2)$print()
#> 
#>       One-Way Test for Equal Means 
#> 
#> scoring: none    type: permu(756756)    method: default
#> statistic = 11552.89, p-value = 0.05127412

t$type <- "asymp"
t
#> 
#>       One-Way Test for Equal Means 
#> 
#> scoring: none    type: asymp    method: default
#> statistic = 3.781445, p-value = 0.05327082
# }
```
