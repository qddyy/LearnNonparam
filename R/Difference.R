#' @title `r Difference$private_fields$.name`
#' 
#' @description Performs mean/median based two-sample test on samples.
#' 
#' @aliases twosample.difference
#' 
#' @examples
#' pmt(
#'     "twosample.difference", method = "mean",
#'     alternative = "greater", n_permu = 0
#' )$test(Table2.1.1)$print()$plot(
#'     style = "graphic", breaks = seq(-20, 25, length.out = 9)
#' )
#' 
#' pmt(
#'     "twosample.difference", method = "mean",
#'     alternative = "greater", n_permu = 1000
#' )$test(Table2.3.1)$print()
#' 
#' @export
#' 
#' @importFrom R6 R6Class


Difference <- R6Class(
    classname = "Difference",
    inherit = TwoSampleLocationTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `Difference` object.
        #' 
        #' @template pmt_init_params
        #' @template location_init_params
        #' @param method a character string specifying whether to use the mean or the median.
        #' 
        #' @return A `Difference` object.
        initialize = function(
            method = c("mean", "median"),
            alternative = c("two_sided", "less", "greater"),
            null_value = 0, n_permu = 1e4
        ) {
            self$method <- method
            self$alternative <- alternative
            self$null_value <- null_value
            self$n_permu <- n_permu
        }
    ),
    private = list(
        .name = "Two-Sample Test Based on Mean or Median",

        .define = function() {
            private$.statistic_func <- function(x, y) {
                m <- length(x)
                n <- length(y)

                switch(private$.method,
                    mean = function(x, y) sum(x) / m - sum(y) / n,
                    median = {
                        median_x <- make_median(m)
                        median_y <- make_median(n)
                        function(x, y) median_x(x) - median_y(y)
                    }
                )
            }
        }
    )
)

make_median <- function(length) {
    half <- (length + 1L) %/% 2L
    if (length %% 2L == 1L) {
        function(v) {
            sorted <- sort.int(v, partial = half)
            sorted[half]
        }
    } else {
        function(v) {
            sorted <- sort.int(v, partial = half + 0L:1L)
            (sorted[half] + sorted[half + 1L]) / 2
        }
    }
}