#' @title `r CDF$private_fields$.name`
#' 
#' @description Performs statistical inference on population cdf.
#' 
#' @aliases onesample.cdf
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom stats qnorm plot.stepfun


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

        #' @description Plot the estimate and confidence bounds for population cdf of the data.
        #' 
        #' @template plot_params
        #' 
        #' @return The object itself (invisibly).
        plot = function(style = c("graphics", "ggplot2")) {
            private$.type <- "permu"
            super$plot(style = style)
            private$.type <- "asymp"

            invisible(self)
        }
    ),
    private = list(
        .name = "Cumulative Distribution Function",

        .type = "asymp",

        .lims_for_plot = NULL,

        .calculate_extra = function() {
            n <- length(private$.data)
            sorted <- sort(private$.data)

            F_n <- seq.int(0, n) / n
            private$.estimate <- stepfun(sorted, F_n, right = TRUE)

            A <- 1 / sqrt(n) * qnorm(1 - (1 - private$.conf_level) / 2)
            delta_n <- A * sqrt(F_n * (1 - F_n))
            private$.ci <- list(
                lower = stepfun(sorted, F_n - delta_n, right = TRUE),
                upper = stepfun(sorted, F_n + delta_n, right = TRUE)
            )

            private$.lims_for_plot <- list(
                x = c(sorted[1], get_last(sorted)),
                y = c(min(F_n - delta_n), max(F_n + delta_n))
            )
        },

        .print = function(...) {},

        .plot = function(...) {
            plot.stepfun(
                private$.estimate, lty = "solid",
                xlim = private$.lims_for_plot$x,
                ylim = private$.lims_for_plot$y,
                main = "Empirical CDF with Confidence Bounds",
                xlab = expression(x), ylab = expression(F[n](x))
            )
            plot.stepfun(private$.ci$lower, lty = "dashed", add = TRUE)
            plot.stepfun(private$.ci$upper, lty = "dashed", add = TRUE)
        },

        .autoplot = function(...) {
            ggplot2::ggplot(
                data = data.frame(
                    x = private$.data,
                    ecdf = private$.estimate(private$.data),
                    lower = private$.ci$lower(private$.data),
                    upper = private$.ci$upper(private$.data)
                ), mapping = ggplot2::aes(x = .data$x)
            ) +
                ggplot2::geom_step(
                    mapping = ggplot2::aes(y = .data$ecdf), linetype = "solid"
                ) +
                ggplot2::geom_step(
                    mapping = ggplot2::aes(y = .data$lower), linetype = "dashed"
                ) +
                ggplot2::geom_step(
                    mapping = ggplot2::aes(y = .data$upper), linetype = "dashed"
                ) +
                ggplot2::lims(
                    x = private$.lims_for_plot$x, y = private$.lims_for_plot$y
                ) +
                ggplot2::labs(
                    title = "Empirical CDF with Confidence Bounds",
                    x = expression(x), y = expression(F[n](x))
                ) +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
                )
        }
    )
)