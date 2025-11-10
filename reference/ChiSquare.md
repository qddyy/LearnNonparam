# Chi-Square Test on Contingency Table

Performs chi-square test on contingency tables.

## Super classes

[`LearnNonparam::PermuTest`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.md)
-\>
[`LearnNonparam::ContingencyTableTest`](https://qddyy.github.io/LearnNonparam/reference/ContingencyTableTest.md)
-\> `ChiSquare`

## Methods

### Public methods

- [`ChiSquare$new()`](#method-ChiSquare-new)

Inherited methods

- [`LearnNonparam::PermuTest$plot()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-plot)
- [`LearnNonparam::PermuTest$print()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-print)
- [`LearnNonparam::PermuTest$test()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-test)

------------------------------------------------------------------------

### Method `new()`

Create a new `ChiSquare` object.

#### Usage

    ChiSquare$new(type = c("permu", "asymp"), n_permu = 10000)

#### Arguments

- `type`:

  a character string specifying the way to calculate the p-value.

- `n_permu`:

  an integer indicating number of permutations for the permutation
  distribution. If set to `0`, all permutations will be used.

#### Returns

A `ChiSquare` object.

## Examples

``` r
t <- pmt(
    "table.chisq", n_permu = 0
)$test(Table5.4.2)$print()
#> 
#>       Chi-Square Test on Contingency Table 
#> 
#> scoring: none    type: permu(35)    method: default
#> statistic = 4.277778, p-value = 0.3142857

t$type <- "asymp"
t
#> 
#>       Chi-Square Test on Contingency Table 
#> 
#> scoring: none    type: asymp    method: default
#> statistic = 4.277778, p-value = 0.1177856
```
