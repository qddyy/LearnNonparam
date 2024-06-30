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
        #' @template pmt_init_params
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
            yx_length_ratio <- length(private$.data$y) / length(private$.data$x)

            private$.statistic_func <- function(x, y) {
                sum(x) / sum(y) * yx_length_ratio
            }
        },

        .calculate_statistic = function() {
            super$.calculate_statistic()

            if (private$.alternative == "two_sided") {
                private$.statistic <- `attr<-`(
                    max_min_ratio(private$.statistic), "permu",
                    max_min_ratio(attr(private$.statistic, "permu"))
                )
            }
        },

        .calculate_side = function() {
            private$.side <- if (private$.alternative == "less") "l" else "r"
        },

        .on_alternative_change = function() private$.calculate()
    )
)

max_min_ratio <- function(ratio) ratio^(2 * (ratio >= 1) - 1)