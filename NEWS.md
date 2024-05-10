# LearnNonparam v1.2.1

- R side
  - Added `define_pmt`
  - Added some error messages
  - Added some active bindings
  - Introduced `graphics` as an alternative to `ggplot2`
  - Introduced `compiler::cmpfun` for better performance
  - Renamed `ANOVA`, `RCBD`, `RCBDANOVA` and `SignedDiff`
  - Replaced `MultiCompT` and `TukeyHSD` with `Studentized`
  - Replaced `"approx"` with `"asymp"`
  - Improved `MultipleComparison`
  - Improved `CDF$plot()`
  - Fixed many bugs
- C++ side
  - A new progress bar
  - Support for larger `n_permu`

# LearnNonparam v1.2.0

- Added logo
- Introduced [Rcpp](https://CRAN.R-project.org/package=Rcpp)
- Replaced `Mean` with `Difference`
- Replaced `SignedScore` with `SignedDiff`
- Replaced `PermuTest$feed()` with `PermuTest$test()`
- Accelerated `RCBD` and `TwoSamplePairedTest`
- Improved `print()` and `plot()` method of `CDF`, `MultipleComparison` and `PermuTest`
- Added `"approx"` option for the `Quantile$type`
- Fixed many bugs