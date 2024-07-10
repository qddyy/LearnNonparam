# LearnNonparam 1.2.2

- R side
    - Changed the default value of `n_permu` to `1e4`
    - Added support for test statistics defined using Rcpp in `define_pmt`
    - Added support for `null_value` other than zero in two-sample location tests
    - Added support for `scoring` in `TwoSampleAssociationTest`
    - Added a confidence interval for p-value in `print()`
    - Added the `LearnNonparam.pmt_progress` option
    - Added `attr(t$n_permu, "n_used")`
    - Replaced `multicomp.*` with `multcomp.*`
    - Replaced `KSampleF` and `RCBDF` with `OneWay` and `RCBDOneWay`
    - Stopped exporting abstract classes (`TwoSampleTest`, `KSampleTest`, etc.)
    - Improved the efficiency of `ChiSquare`, `Difference`, `JonckheereTerpstra`, `KruskalWallis`, `RatioMeanDeviance` and `Studentized`
    - Fixed many bugs

- C++ side
    - Introduced a new progress bar, which is built at compile time to minimize runtime overhead

# LearnNonparam 1.2.1

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

# LearnNonparam 1.2.0

- Added logo
- Introduced [Rcpp](https://CRAN.R-project.org/package=Rcpp)
- Replaced `Mean` with `Difference`
- Replaced `SignedScore` with `SignedDiff`
- Replaced `PermuTest$feed()` with `PermuTest$test()`
- Accelerated `RCBD` and `TwoSamplePairedTest`
- Improved `print()` and `plot()` method of `CDF`, `MultipleComparison` and `PermuTest`
- Added `"approx"` option for the `Quantile$type`
- Fixed many bugs
