# Multiple Comparison Based on Studentized Statistic

Performs studentized statistic based multiple comparison on samples.

## Super classes

[`LearnNonparam::PermuTest`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.md)
-\>
[`LearnNonparam::KSampleTest`](https://qddyy.github.io/LearnNonparam/reference/KSampleTest.md)
-\>
[`LearnNonparam::MultipleComparison`](https://qddyy.github.io/LearnNonparam/reference/MultipleComparison.md)
-\> `Studentized`

## Methods

### Public methods

- [`Studentized$new()`](#method-Studentized-new)

Inherited methods

- [`LearnNonparam::PermuTest$plot()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-plot)
- [`LearnNonparam::PermuTest$print()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-print)
- [`LearnNonparam::PermuTest$test()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-test)

------------------------------------------------------------------------

### Method `new()`

Create a new `Studentized` object.

#### Usage

    Studentized$new(
      type = c("permu", "asymp"),
      method = c("bonferroni", "tukey"),
      scoring = c("none", "rank", "vw", "expon"),
      conf_level = 0.95,
      n_permu = 10000
    )

#### Arguments

- `type`:

  a character string specifying the way to calculate the p-value.

- `method`:

  a character string specifying whether to use Bonferroni's method or
  Tukey's HSD method.

- `scoring`:

  a character string specifying the scoring system.

- `conf_level`:

  a number between zero and one indicating the family-wise confidence
  level to use.

- `n_permu`:

  an integer indicating number of permutations for the permutation
  distribution. If set to `0`, all permutations will be used.

#### Returns

A `Studentized` object.

## Examples

``` r
t <- pmt(
    "multcomp.studentized", method = "bonferroni"
)$test(Table3.3.1)$print()
#> 
#>   Multiple Comparison Based on Studentized Statistic 
#> 
#> scoring: none    type: permu(10000)    method: bonferroni
#> 
#> family-wise confidence level: 95%
#> 
#>                           statistic p-value  
#> location_1 ~ location_2  0.02737145  0.9790  
#> location_1 ~ location_3 -1.77914406  0.0908  
#> location_1 ~ location_4 -3.41048230  0.0036 *
#> location_2 ~ location_3 -1.80651550  0.0878  
#> location_2 ~ location_4 -3.43785375  0.0044 *
#> location_3 ~ location_4 -1.63133824  0.1228  

t$type <- "asymp"
t
#> 
#>   Multiple Comparison Based on Studentized Statistic 
#> 
#> scoring: none    type: asymp    method: bonferroni
#> 
#> family-wise confidence level: 95%
#> 
#>                           statistic     p-value  
#> location_1 ~ location_2  0.02737145 0.978434752  
#> location_1 ~ location_3 -1.77914406 0.090419680  
#> location_1 ~ location_4 -3.41048230 0.002773484 *
#> location_2 ~ location_3 -1.80651550 0.085909110  
#> location_2 ~ location_4 -3.43785375 0.002603664 *
#> location_3 ~ location_4 -1.63133824 0.118466249  

t$scoring <- "rank"
t
#> 
#>   Multiple Comparison Based on Studentized Statistic 
#> 
#> scoring: rank    type: asymp    method: bonferroni
#> 
#> family-wise confidence level: 95%
#> 
#>                          statistic     p-value  
#> location_1 ~ location_2  0.1224745 0.902523250  
#> location_1 ~ location_3 -1.3880442 0.165123590  
#> location_1 ~ location_4 -2.6536139 0.007963489 *
#> location_2 ~ location_3 -1.5105187 0.130911128  
#> location_2 ~ location_4 -2.7760884 0.005501725 *
#> location_3 ~ location_4 -1.2655697 0.205667180  

t$method <- "tukey"
t
#> 
#>   Multiple Comparison Based on Studentized Statistic 
#> 
#> scoring: rank    type: asymp    method: tukey
#> 
#> family-wise confidence level: 95%
#> 
#>                         statistic    p-value  
#> location_1 ~ location_2 0.1732051 0.99934385  
#> location_1 ~ location_3 1.9629909 0.50683497  
#> location_1 ~ location_4 3.7527767 0.03978340 *
#> location_2 ~ location_3 2.1361960 0.43109997  
#> location_2 ~ location_4 3.9259818 0.02817015 *
#> location_3 ~ location_4 1.7897858 0.58494735  

t$scoring <- "none"
t
#> 
#>   Multiple Comparison Based on Studentized Statistic 
#> 
#> scoring: none    type: asymp    method: tukey
#> 
#> family-wise confidence level: 95%
#> 
#>                          statistic    p-value  
#> location_1 ~ location_2 0.03870907 0.99999236  
#> location_1 ~ location_3 2.51608965 0.31194094  
#> location_1 ~ location_4 4.82315032 0.01356971 *
#> location_2 ~ location_3 2.55479873 0.29951264  
#> location_2 ~ location_4 4.86185939 0.01277452 *
#> location_3 ~ location_4 2.30706067 0.38453323  

t$type <- "permu"
t
#> 
#>   Multiple Comparison Based on Studentized Statistic 
#> 
#> scoring: none    type: permu(10000)    method: tukey
#> 
#> family-wise confidence level: 95%
#> 
#>                          statistic p-value  
#> location_1 ~ location_2 0.03870907  1.0000  
#> location_1 ~ location_3 2.51608965  0.3133  
#> location_1 ~ location_4 4.82315032  0.0133 *
#> location_2 ~ location_3 2.55479873  0.2994  
#> location_2 ~ location_4 4.86185939  0.0122 *
#> location_3 ~ location_4 2.30706067  0.3843  
```
