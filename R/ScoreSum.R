#' @title `r ScoreSum$private_fields$.name`
#' 
#' @description Performs sum of scores based two-sample test on samples. It is almost the same as two-sample wilcoxon rank sum test but uses more scoring systems.
#' 
#' @aliases twosample.scoresum
#' 
#' @examples
#' pmt(
#'     "twosample.scoresum", scoring = "expon",
#'     alternative = "greater", n_permu = 0
#' )$test(Table2.6.2)$print()
#' 
#' @export
#' 
#' @importFrom R6 R6Class


ScoreSum <- R6Class(
    classname = "ScoreSum",
    inherit = TwoSampleLocationTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `ScoreSum` object.
        #' 
        #' @template pmt_init_params
        #' @template location_init_params
        #' 
        #' @return A `ScoreSum` object.
        initialize = function(
            scoring = c("rank", "vw", "expon"),
            alternative = c("two_sided", "less", "greater"),
            null_value = 0, n_permu = 1e4
        ) {
            self$scoring <- scoring
            self$alternative <- alternative
            self$null_value <- null_value
            self$n_permu <- n_permu
        }
    ),
    private = list(
        .name = "Two-Sample Test Based on Sum of Scores",

        .define = function() {
            private$.statistic_func <- function(x, y) sum(x)
        }
    )
)