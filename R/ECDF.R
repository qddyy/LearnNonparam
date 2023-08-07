#' @title Empirical Cumulative Distribution Function
#' 
#' @description This is the abstract base class for empirical cumulative distribution function objects. 
#' 
#' 
#' @export
#' 
#' @import ggplot2
#' @importFrom R6 R6Class


ECDF <- R6Class(
    classname = "Empirical Cumulative Distribution Function",
    inherit = OneSampleTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `ECDF` object. 
        #' 
        #' @param conf_level a number specifying confidence level of the interval.
        #' 
        #' @return A `ECDF` object. 
        initialize = function(conf_level = 0.95) {
            private$.conf_level <- conf_level
        },

        #' @description Draw the empirical cumulative distribution function of the data fed (with confidence bounds). 
        #' 
        #' @return The object itself (invisibly). 
        plot_ecdf = function() {
            ecdf <- ggplot() +
                stat_function(fun = private$.estimate, geom = "step") +
                stat_function(fun = private$.ci$lower, geom = "step", linetype = 2) +
                stat_function(fun = private$.ci$upper, geom = "step", linetype = 2) +
                xlim(c(min(private$.data), max(private$.data))) +
                labs(x = "", y = "")
            print(ecdf)

            invisible(self)
        }
    ),
    private = list(
        .calculate_extra = function() {
            private$.estimate <- F_n <- ecdf(private$.data)

            n <- length(private$.data)
            A <- 1 / sqrt(n) * qnorm(1 - (1 - private$.conf_level) / 2)
            delta_n <- function(x) A * sqrt(F_n(x) * (1 - F_n(x)))

            private$.ci <- list(
                lower = function(x) F_n(x) - delta_n(x),
                upper = function(x) F_n(x) + delta_n(x)
            )
        }
    )
)