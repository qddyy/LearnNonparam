# Ansari-Bradley Test

Performs Ansari-Bradley test on samples.

## Super classes

[`LearnNonparam::PermuTest`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.md)
-\>
[`LearnNonparam::TwoSampleTest`](https://qddyy.github.io/LearnNonparam/reference/TwoSampleTest.md)
-\> `AnsariBradley`

## Methods

### Public methods

- [`AnsariBradley$new()`](#method-AnsariBradley-new)

Inherited methods

- [`LearnNonparam::PermuTest$plot()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-plot)
- [`LearnNonparam::PermuTest$print()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-print)
- [`LearnNonparam::PermuTest$test()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-test)

------------------------------------------------------------------------

### Method `new()`

Create a new `AnsariBradley` object.

#### Usage

    AnsariBradley$new(
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

A `AnsariBradley` object.

## Examples

``` r
pmt(
    "twosample.ansari",
    alternative = "greater", n_permu = 0
)$test(Table2.8.1)$print()
#> 
#>       Ansari-Bradley Test 
#> 
#> scoring: Ansari-Bradley rank    type: permu(252)    method: default
#> statistic = 13, p-value = 0.2698413
#> alternative hypothesis: true ratio of scales is greater than 1
```
