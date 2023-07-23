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

        #' @description Prepare the object for plotting. 
        #' 
        #' @param plot a logical indicating whether to show the histogram.
        #' 
        #' @return A ggplot object containing the empirical distribution with lower and upper confidence bounds (invisibly). 
        ggplot = function(plot = TRUE) {
            lines <- ggplot() +
                stat_function(fun = private$.ecdf, geom = "step") +
                stat_function(fun = private$.lower, geom = "step", linetype = 2) +
                stat_function(fun = private$.upper, geom = "step", linetype = 2) +
                xlim(c(min(private$.data), max(private$.data)))
                labs(x = "", y = "")

            if (plot) print(lines)

            invisible(lines)
        }
    ),
    private = list(
        .ecdf = NULL,
        .lower = NULL,
        .upper = NULL,

        .calculate = function() {
            F_n <- ecdf(private$.data)

            n <- length(private$.data)
            A <- 1 / sqrt(n) * qnorm(1 - (1 - private$.conf_level) / 2)
            delta_n <- function(x) A * sqrt(F_n(x) * (1 - F_n(x)))

            private$.ecdf <- F_n
            private$.upper <- function(x) F_n(x) + delta_n(x) 
            private$.lower <- function(x) F_n(x) - delta_n(x)
        }
    ),
    active = list(
        #' @field ecdf Empirical distribution. 
        ecdf = function() private$.ecdf,
        #' @field upper confidence bound. 
        lower = function() private$.lower,
        #' @field lower confidence bound. 
        upper = function() private$.upper
    )
)