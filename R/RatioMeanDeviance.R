#' @title Ratio Mean Deviance Test
#' 
#' @description Performs two sample ratio mean deviance test on data vectors. 
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class


RatioMeanDeviance <- R6Class(
    classname = "Ratio Mean Deviance Test",
    inherit = TwoSampleTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `RatioMeanDeviance` object. 
        #' 
        #' @param alternative a character string specifying the alternative hypothesis, must be one of `"two_sided"` (default), `"greater"` or `"less"`.
        #' @param n_permu an integer specifying how many permutations should be used to construct the permutation distribution. If `NULL` (default) then all permutations are used.
        #' 
        #' @return A `RatioMeanDeviance` object. 
        initialize = function(alternative = c("two_sided", "less", "greater"), n_permu = NULL) {
            super$initialize(alternative = match.arg(alternative), n_permu = n_permu)

            private$.statistic_func <- function(x, y) mean(x) / mean(y)
        }
    ),
    private = list(
        .calculate = function() {
            x <- private$.data$x
            y <- private$.data$y

            private$.data <- list(x = abs(x - median(x)), y = abs(y - median(y)))

            super$.calculate()

            private$.data <- list(x = x, y = y)
        }
    )
)