#' @title `r RatioMeanDeviance$private_fields$.name`
#' 
#' @description Performs two sample ratio mean deviance test on data vectors. 
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
        initialize = function(alternative = c("two_sided", "less", "greater"), n_permu = 0L) {
            super$initialize(null_value = 1, alternative = match.arg(alternative), n_permu = n_permu)

            private$.scoring <- "dev"
        }
    ),
    private = list(
        .name = "Ratio Mean Deviance Test",
        .param_name = "ratio of scales",

        .calculate_score = function() {
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