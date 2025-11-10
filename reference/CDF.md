# Inference on Cumulative Distribution Function

Performs statistical inference on population cumulative distribution
function.

## Super classes

[`LearnNonparam::PermuTest`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.md)
-\>
[`LearnNonparam::OneSampleTest`](https://qddyy.github.io/LearnNonparam/reference/OneSampleTest.md)
-\> `CDF`

## Methods

### Public methods

- [`CDF$new()`](#method-CDF-new)

- [`CDF$plot()`](#method-CDF-plot)

Inherited methods

- [`LearnNonparam::PermuTest$print()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-print)
- [`LearnNonparam::PermuTest$test()`](https://qddyy.github.io/LearnNonparam/reference/PermuTest.html#method-test)

------------------------------------------------------------------------

### Method `new()`

Create a new `CDF` object.

#### Usage

    CDF$new(method = c("binomial", "dkw"), conf_level = 0.95)

#### Arguments

- `method`:

  a character string specifying whether to use a confidence band based
  on the binomial distribution or the Dvoretzky-Kiefer-Wolfowitz
  inequality.

- `conf_level`:

  a number specifying confidence level of the confidence bounds.

#### Returns

A `CDF` object.

------------------------------------------------------------------------

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

Plot the estimate and confidence bounds for population cumulative
distribution function.

#### Usage

    CDF$plot(style = c("graphics", "ggplot2"))

#### Arguments

- `style`:

  a character string specifying which package to use.

#### Returns

The object itself (invisibly).

## Examples

``` r
pmt("onesample.cdf")$test(Table1.2.1)$plot(style = "graphic")

```
