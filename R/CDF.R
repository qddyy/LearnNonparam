#' @title `r CDF$private_fields$.name`
#' 
#' @description Performs statistical inference on population cumulative distribution function.
#' 
#' @aliases onesample.cdf
#' 
#' @examples
#' pmt("onesample.cdf")$test(Table1.2.1)$plot(style = "graphic")
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom stats qnorm stepfun plot.stepfun


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
        initialize = function(
            conf_level = 0.95
        ) {
            self$conf_level <- conf_level
        },

        #' @description Plot the estimate and confidence bounds for population cumulative distribution function.
        #' 
        #' @template plot_params
        #' 
        #' @return The object itself (invisibly).
        plot = function(style = c("graphics", "ggplot2")) {
            if (is.null(private$.raw_data)) {
                warning("Must provide a sample before calling the plot method")
            } else if (match.arg(style) == "graphics") {
                private$.plot()
            } else {
                requireNamespace("ggplot2")
                print(private$.autoplot())
            }

            invisible(self)
        }
    ),
    private = list(
        .name = "Inference on Cumulative Distribution Function",

        .type = "asymp",

        .lims_for_plot = NULL,

        .preprocess = function() {
            super$.preprocess()

            private$.data <- sort.int(private$.data)
        },

        .calculate_side = function() NULL,

        .calculate_extra = function() {
            n <- length(private$.data)

            F_n <- seq.int(0, n) / n
            private$.estimate <- stepfun(private$.data, F_n)

            A <- 1 / sqrt(n) * qnorm(1 - (1 - private$.conf_level) / 2)
            delta_n <- A * sqrt(F_n * (1 - F_n))
            private$.conf_int <- list(
                lower = stepfun(private$.data, F_n - delta_n),
                upper = stepfun(private$.data, F_n + delta_n)
            )

            private$.lims_for_plot <- list(
                x = c(private$.data[1], private$.data[n]),
                y = c(min(F_n - delta_n), max(F_n + delta_n))
            )
        },

        .print = function() {
            cat(format(self), sep = "\n")
        },

        .plot = function() {
            plot.stepfun(
                private$.estimate,
                lty = "solid", do.points = FALSE,
                xlim = private$.lims_for_plot$x,
                ylim = private$.lims_for_plot$y,
                main = paste(
                    "Empirical CDF with",
                    paste0(private$.conf_level * 100, "%"),
                    "Confidence Bounds"
                ), xlab = expression(x), ylab = expression(F[n](x))
            )
            plot.stepfun(
                private$.conf_int$lower,
                lty = "dashed", do.points = FALSE, add = TRUE
            )
            plot.stepfun(
                private$.conf_int$upper,
                lty = "dashed", do.points = FALSE, add = TRUE
            )
        },

        .autoplot = function() {
            ggplot2::ggplot() +
                ggplot2::geom_step(
                    mapping = ggplot2::aes(x = .data$x, y = .data$ecdf),
                    data = data.frame(
                        x = c(private$.data[1], private$.data),
                        ecdf = c(0, private$.estimate(private$.data))
                    ), linetype = "solid"
                ) +
                ggplot2::geom_rect(
                    mapping = ggplot2::aes(
                        xmin = .data$xmin, xmax = .data$xmax,
                        ymin = .data$ymin, ymax = .data$ymax
                    ),
                    data = {
                        n <- length(private$.data)
                        data.frame(
                            xmin = private$.data[-n],
                            xmax = private$.data[-1],
                            ymin = private$.conf_int$lower(private$.data[-n]),
                            ymax = private$.conf_int$upper(private$.data[-n])
                        )
                    }, alpha = 0.25
                ) +
                ggplot2::geom_hline(yintercept = c(0, 1), linetype = "dashed") +
                ggplot2::lims(
                    x = private$.lims_for_plot$x, y = private$.lims_for_plot$y
                ) +
                ggplot2::labs(
                    title = paste(
                        "Empirical CDF with",
                        paste0(private$.conf_level * 100, "%"),
                        "Confidence Bounds"
                    ), x = expression(x), y = expression(F[n](x))
                ) +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(
                        face = "bold", hjust = 0.5
                    )
                )
        }
    )
)