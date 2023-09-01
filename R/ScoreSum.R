#' @title `r ScoreSum$private_fields$.name`
#' 
#' @description Performs two sample score sum test on data vectors, which is almost the same as wilcoxon rank sum test but uses not only rank but also scoring systems other than it.
#' 
#' @aliases twosample.scoresum
#' 
#' @export
#' 
#' @importFrom R6 R6Class


ScoreSum <- R6Class(
    classname = "ScoreSum",
    inherit = TwoSampleTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `ScoreSum` object. 
        #' 
        #' @template init_params
        #' 
        #' @return A `ScoreSum` object. 
        initialize = function(alternative = c("two_sided", "less", "greater"), n_permu = NULL, scoring = c("rank", "vw", "expon")) {
            super$initialize(scoring = match.arg(scoring), alternative = match.arg(alternative), n_permu = n_permu)
        }
    ),
    private = list(
        .name = "Score Sum Test",

        .define_statistic = function() {
            private$.statistic_func <- function(x, y) sum(x)
        }
    )
)