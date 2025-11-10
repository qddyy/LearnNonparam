# Kruskal-Wallis Test

Performs Kruskal-Wallis test on samples.

## Super classes

[`LearnNonparam::PermuTest`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.md)
-\>
[`LearnNonparam::KSampleTest`](https://qddyy.github.io/LearnNonparam/reference/KSampleTest.md)
-\> `KruskalWallis`

## Methods

### Public methods

- [`KruskalWallis$new()`](#method-KruskalWallis-new)

Inherited methods

- [`LearnNonparam::PermuTest$plot()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-plot)
- [`LearnNonparam::PermuTest$print()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-print)
- [`LearnNonparam::PermuTest$test()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-test)

------------------------------------------------------------------------

### Method `new()`

Create a new `KruskalWallis` object.

#### Usage

    KruskalWallis$new(
      type = c("permu", "asymp"),
      scoring = c("rank", "vw", "expon"),
      n_permu = 10000
    )

#### Arguments

- `type`:

  a character string specifying the way to calculate the p-value.

- `scoring`:

  a character string specifying the scoring system.

- `n_permu`:

  an integer indicating number of permutations for the permutation
  distribution. If set to `0`, all permutations will be used.

#### Returns

A `KruskalWallis` object.

## Examples

``` r
pmt(
    "ksample.kw", type = "asymp"
)$test(Table3.2.2)$print()
#> 
#>       Kruskal-Wallis Test 
#> 
#> scoring: rank    type: asymp    method: default
#> statistic = 17.14286, p-value = 0.0006605027

t <- pmt(
    "ksample.kw", type = "permu"
)$test(Table3.2.3)$print()
#> 
#>       Kruskal-Wallis Test 
#> 
#> scoring: rank    type: permu(10000)    method: default
#> statistic = 8.246588, p-value = 0.0101 (± 0.001959767 at 95% confidence)

t$type <- "asymp"
t
#> 
#>       Kruskal-Wallis Test 
#> 
#> scoring: rank    type: asymp    method: default
#> statistic = 8.246588, p-value = 0.01619109
```
