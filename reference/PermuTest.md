# PermuTest Class

Abstract class for permutation tests.

## Active bindings

- `type`:

  The way to calculate the p-value.

- `method`:

  The method used.

- `scoring`:

  The scoring system used.

- `alternative`:

  The alternative hypothesis.

- `null_value`:

  The hypothesized value of the parameter in the null hypothesis.

- `conf_level`:

  The confidence level of the interval.

- `n_permu`:

  The number of permutations used.

- `data`:

  The data.

- `statistic`:

  The test statistic.

- `p_value`:

  The p-value.

- `estimate`:

  The estimated value of the parameter.

- `conf_int`:

  The confidence interval of the parameter.

## Methods

### Public methods

- [`PermuTest$test()`](#method-PermuTest-test)

- [`PermuTest$print()`](#method-PermuTest-print)

- [`PermuTest$plot()`](#method-PermuTest-plot)

------------------------------------------------------------------------

### Method `test()`

Perform test on sample(s).

#### Usage

    PermuTest$test(...)

#### Arguments

- `...`:

  sample(s). Can be numeric vector(s) or a `data.frame` or `list`
  containing them.

#### Details

A progress bar is shown by default. Use
`options(LearnNonparam.pmt_progress = FALSE)` to disable it.

#### Returns

The object itself (invisibly).

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the results of the test.

#### Usage

    PermuTest$print()

#### Returns

The object itself (invisibly).

------------------------------------------------------------------------

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

Plot histogram(s) of the permutation distribution. Note that this method
only works if `type` is set to `"permu"`.

#### Usage

    PermuTest$plot(style = c("graphics", "ggplot2"), ...)

#### Arguments

- `style`:

  a character string specifying which package to use.

- `...`:

  passed to
  [`graphics::hist.default()`](https://rdrr.io/r/graphics/hist.html) or
  [`ggplot2::stat_bin()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html).

#### Returns

The object itself (invisibly).
