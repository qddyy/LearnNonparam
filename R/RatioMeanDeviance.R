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
        initialize = function(alternative = c("two_sided", "less", "greater"), n_permu = NULL) {
            super$initialize(alternative = match.arg(alternative), n_permu = n_permu)

            private$.scoring <- "dev"
        }
    ),
    private = list(
        .name = "Ratio Mean Deviance Test",

        .calculate_score = function() {
            private$.data <- list(
                x = abs(private$.data$x - median(private$.data$x)),
                y = abs(private$.data$y - median(private$.data$y))
            )
        },

        .define_statistic = function() {
            private$.statistic_func <- function(x, y) mean(x) / mean(y)
        }
    )
)