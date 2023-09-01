#' @title `r Mean$private_fields$.name`
#' 
#' @description Performs mean based two sample permutation test on data vectors. 
#' 
#' @aliases twosample.mean
#' 
#' @export
#' 
#' @importFrom R6 R6Class


Mean <- R6Class(
    classname = "Mean",
    inherit = TwoSampleTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `Mean` object. 
        #' 
        #' @template init_params
        #' 
        #' @return A `Mean` object. 
        initialize = function(alternative = c("two_sided", "less", "greater"), n_permu = NULL) {
            super$initialize(alternative = match.arg(alternative), n_permu = n_permu)
        }
    ),
    private = list(
        .name = "Two Sample Test Based on Mean",

        .define_statistic = function() {
            private$.statistic_func <- function(x, y) mean(x) - mean(y)
        }
    )
)