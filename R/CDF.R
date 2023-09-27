#' @title `r CDF$private_fields$.name`
#' 
#' @description Performs statistical inference on population cdf. 
#' 
#' @aliases onesample.cdf
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom ggplot2 ggplot geom_step xlim labs theme element_text


CDF <- R6Class(
    classname = "CDF",
    inherit = OneSampleTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `CDF` object. 
        #' 
        #' @param conf_level a number specifying confidence level of the confidence bounds.
        #' 
        #' @return A `CDF` object. 
        initialize = function(conf_level = 0.95) {
            super$initialize(conf_level = conf_level)
        },

        #' @description Plot the estimate and confidence bounds for population cdf of the data fed. 
        #' 
        #' @return The object itself (invisibly). 
        plot = function() {
            if (!is.null(private$.raw_data)) {
                private$.plot()
            }

            invisible(self)
        }
    ),
    private = list(
        .name = "Cumulative Distribution Function",

        .type = "approx",

        .print = function(...) {},

        .plot = function() {
            cdf_with_bounds <- ggplot(
                data = data.frame(
                    cdf = private$.estimate(private$.data),
                    lower = private$.ci$lower(private$.data),
                    upper = private$.ci$upper(private$.data)
                ), mapping = aes(x = private$.data)
            ) +
                geom_step(mapping = aes(y = cdf)) +
                geom_step(mapping = aes(y = lower), linetype = 2) +
                geom_step(mapping = aes(y = upper), linetype = 2) +
                xlim(c(min(private$.data), max(private$.data))) +
                labs(
                    title = "Empirical CDF with Confidence Bounds",
                    x = expression(x), y = expression(F[n](x))
                ) +
                theme(plot.title = element_text(face = "bold", hjust = 0.5))
            print(cdf_with_bounds)
        },

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