#' @title `r RatioMeanDeviance$private_fields$.name`
#' 
#' @description Performs ratio mean deviance test on samples.
#' 
#' @aliases twosample.rmd
#' 
#' @export
#' 
#' @importFrom R6 R6Class


RatioMeanDeviance <- R6Class(
    classname = "RatioMeanDeviance",
    inherit = TwoSampleTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `RatioMeanDeviance` object.
        #' 
        #' @template init_params
        #' 
        #' @return A `RatioMeanDeviance` object.
        initialize = function(
            alternative = c("two_sided", "less", "greater"),
            n_permu = 1e4
        ) {
            self$alternative <- alternative
            self$n_permu <- n_permu
        }
    ),
    private = list(
        .name = "Ratio Mean Deviance Test",
        .param_name = "ratio of scales",

        .null_value = 1,

        .preprocess = function() {
            super$.preprocess()

            private$.data <- list(
                x = abs(private$.data$x - median(private$.data$x)),
                y = abs(private$.data$y - median(private$.data$y))
            )
        },

        .define = function() {
            private$.statistic_func <- function(x, y) mean(x) / mean(y)
        }
    )
)