#' @title Score Sum Test
#' 
#' @description Performs two sample score sum test on data vectors, which is almost the same as wilcoxon rank sum test but uses not only rank but also scoring systems other than it.
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class


ScoreSum <- R6Class(
    classname = "Score Sum Test",
    inherit = TwoSampleTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `ScoreSum` object. 
        #' 
        #' @param alternative a character string specifying the alternative hypothesis, must be one of `"two_sided"` (default), `"greater"` or `"less"`.
        #' @param n_permu an integer specifying how many permutations should be used to construct the permutation distribution. If `NULL` (default) then all permutations are used.
        #' @param scoring a character string specifying which scoring system to be used, must be one of `"rank"` (default), `"vw"` or `"savage"`.
        #' 
        #' @return A `ScoreSum` object. 
        initialize = function(alternative = c("two_sided", "less", "greater"), n_permu = NULL, scoring = c("rank", "vw", "savage")) {
            super$initialize(scoring = match.arg(scoring), alternative = match.arg(alternative), n_permu = n_permu)

            private$.statistic_func <- function(x, y) sum(x)
        }
    )
)