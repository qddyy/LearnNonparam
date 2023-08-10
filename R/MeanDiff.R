#' @title `r MeanDiff$private_fields$.name`
#' 
#' @description Performs mean based two sample permutation test on data vectors. 
#' 
#' @aliases twosample.mean
#' 
#' @export
#' 
#' @importFrom R6 R6Class


MeanDiff <- R6Class(
    classname = "MeanDiff",
    inherit = TwoSampleTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `MeanDiff` object. 
        #' 
        #' @param alternative a character string specifying the alternative hypothesis, must be one of `"two_sided"` (default), `"greater"` or `"less"`.
        #' @param n_permu an integer specifying how many permutations should be used to construct the permutation distribution. If `NULL` (default) then all permutations are used.
        #' 
        #' @return A `MeanDiff` object. 
        initialize = function(alternative = c("two_sided", "less", "greater"), n_permu = NULL) {
            super$initialize(alternative = match.arg(alternative), n_permu = n_permu)

            private$.statistic_func <- function(x, y) mean(x) - mean(y)
        }
    ),
    private = list(
        .name = "Two Sample Test Based on Mean"
    )
)