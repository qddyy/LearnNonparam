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
        initialize = function(
            scoring = c("rank", "vw", "expon"),
            alternative = c("two_sided", "less", "greater"),
            n_permu = 0L
        ) {
            private$.init(
                scoring = scoring, alternative = alternative, n_permu = n_permu
            )
        }
    ),
    private = list(
        .name = "Score Sum Test",

        .define = function() {
            private$.statistic_func <- function(x, y) sum(x)
        }
    )
)