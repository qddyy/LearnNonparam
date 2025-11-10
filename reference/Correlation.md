# Test for Association Between Paired Samples

Performs correlation coefficient based two-sample association test on
samples.

## Super classes

[`LearnNonparam::PermuTest`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.md)
-\>
[`LearnNonparam::TwoSampleTest`](https://qddyy.github.io/LearnNonparam/reference/TwoSampleTest.md)
-\>
[`LearnNonparam::TwoSamplePairedTest`](https://qddyy.github.io/LearnNonparam/reference/TwoSamplePairedTest.md)
-\>
[`LearnNonparam::TwoSampleAssociationTest`](https://qddyy.github.io/LearnNonparam/reference/TwoSampleAssociationTest.md)
-\> `Correlation`

## Methods

### Public methods

- [`Correlation$new()`](#method-Correlation-new)

Inherited methods

- [`LearnNonparam::PermuTest$plot()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-plot)
- [`LearnNonparam::PermuTest$print()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-print)
- [`LearnNonparam::PermuTest$test()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-test)

------------------------------------------------------------------------

### Method `new()`

Create a new `Correlation` object.

#### Usage

    Correlation$new(
      type = c("permu", "asymp"),
      method = c("pearson", "kendall", "spearman"),
      alternative = c("two_sided", "less", "greater"),
      n_permu = 10000
    )

#### Arguments

- `type`:

  a character string specifying the way to calculate the p-value.

- `method`:

  a character string specifying the correlation coefficient to be used.

- `alternative`:

  a character string specifying the alternative hypothesis.

- `n_permu`:

  an integer indicating number of permutations for the permutation
  distribution. If set to `0`, all permutations will be used.

#### Returns

A `Correlation` object.

## Examples

``` r
pmt(
    "association.corr", method = "pearson",
    alternative = "greater", n_permu = 10000
)$test(Table5.1.2)$print()
#> 
#>       Test for Association Between Paired Samples 
#> 
#> scoring: none    type: permu(10000)    method: pearson
#> statistic = 0.8571756, p-value = 1e-04 (± 0.0001959866 at 95% confidence)
#> alternative hypothesis: true correlation is greater than 0

t <- pmt(
    "association.corr", method = "spearman",
    alternative = "two_sided", n_permu = 10000
)$test(Table5.1.2)$print()
#> 
#>       Test for Association Between Paired Samples 
#> 
#> scoring: rank    type: permu(10000)    method: spearman
#> statistic = 0.8968008, p-value < 2.220446e-16
#> alternative hypothesis: true rho is not equal to 0

t$type <- "asymp"
t
#> 
#>       Test for Association Between Paired Samples 
#> 
#> scoring: rank    type: asymp    method: spearman
#> statistic = 0.8968008, p-value = 4.658861e-07
#> alternative hypothesis: true rho is not equal to 0

t <- pmt(
    "association.corr", method = "kendall",
    alternative = "greater", n_permu = 0
)$test(Table5.2.2)$print()
#> 
#>       Test for Association Between Paired Samples 
#> 
#> scoring: rank    type: permu(37800)    method: kendall
#> statistic = 0.2222222, p-value = 0.187672
#> alternative hypothesis: true tau is greater than 0

t$type <- "asymp"
t
#> 
#>       Test for Association Between Paired Samples 
#> 
#> scoring: rank    type: asymp    method: kendall
#> statistic = 0.2222222, p-value = 0.1697136
#> alternative hypothesis: true tau is greater than 0
```
