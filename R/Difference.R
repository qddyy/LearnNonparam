#' @title `r Difference$private_fields$.name`
#' 
#' @description Performs mean/median based two-sample test on samples.
#' 
#' @aliases twosample.difference
#' 
#' @export
#' 
#' @importFrom R6 R6Class


Difference <- R6Class(
    classname = "Difference",
    inherit = TwoSampleTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `Difference` object.
        #' 
        #' @template init_params
        #' @param method a character string specifying whether to use the mean or the median.
        #' 
        #' @return A `Difference` object.
        initialize = function(
            method = c("mean", "median"),
            alternative = c("two_sided", "less", "greater"),
            n_permu = 0L
        ) {
            self$method <- method
            self$alternative <- alternative
            self$n_permu <- n_permu
        }
    ),
    private = list(
        .name = "Two Sample Test Based on Mean or Median",

        .define = function() {
            private$.statistic_func <- switch(private$.method,
                mean = function(x, y) mean(x) - mean(y),
                median = function(x, y) median(x) - median(y)
            )
        }
    )
)