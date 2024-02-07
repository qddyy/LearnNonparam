#' @title PermuTest Class
#' 
#' @description Abstract class for permutation tests.
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom graphics hist abline


PermuTest <- R6Class(
    classname = "PermuTest",
    cloneable = FALSE,
    public = list(
        initialize = function(...) {
            stop("Can't construct an object from abstract class")
        },

        #' @description Perform test on data.
        #' 
        #' @param ... data to be tested. Can be a `data.frame`, a `list` or numeric vector(s).
        #' 
        #' @return The object itself (invisibly).
        test = function(...) {
            private$.raw_data <- get_data(match.call(), parent.frame())
            private$.calculate()

            invisible(self)
        },

        #' @description Print the results of the test.
        #' 
        #' @param digits an integer specifying the minimum number of significant digits to be printed in values.
        #' 
        #' @return The object itself (invisibly).
        print = function(digits = getOption("digits")) {
            if (!is.null(private$.raw_data)) {
                private$.print(digits = digits)
            }

            invisible(self)
        },

        #' @description Plot histogram(s) of the permutation distribution. Note that it works only if the test's type is `"permu"`.
        #' 
        #' @template plot_params
        #' @param ... extra parameters passed to `graphics::hist` or `ggplot2::stat_bin`.
        #' 
        #' @return The object itself (invisibly).
        plot = function(style = c("graphics", "ggplot2"), ...) {
            if (!is.null(private$.raw_data) & private$.type == "permu") {
                if (match.arg(style) == "graphics") {
                    private$.plot(...)
                } else {
                    requireNamespace("ggplot2")
                    print(private$.autoplot(...))
                }
            }

            invisible(self)
        }
    ),
    private = list(
        .name = NULL,
        .param_name = NULL,

        .type = "permu",
        .method = "default",

        .scoring = "none",

        .n_permu = NULL,

        .data_name = NULL,
        .raw_data = NULL,
        .data = NULL,

        .null_value = NULL,

        .statistic_func = NULL,

        .statistic = NULL,
        .statistic_permu = NULL,

        .alternative = "two_sided",
        .trend = "+",
        .side = NULL,

        .p_value = NULL,

        .estimate = NULL,
        .ci = NULL,
        .conf_level = NULL,

        .calculate = function() {
            private$.preprocess()
            if (private$.scoring != "none") {
                private$.calculate_score()
            }

            private$.define()

            private$.calculate_statistic()

            private$.calculate_side()
            if (private$.type == "permu") {
                private$.calculate_statistic_permu()
                private$.calculate_p_permu()
            } else {
                private$.calculate_p()
            }

            private$.calculate_extra()
        },

        .preprocess = function() {
            # private$.data <- ...
        },

        .calculate_score = function() {
            # private$.data <- ...
        },

        .define = function() {
            # private$.param_name <- ...
            # private$.statistic_func <- ...
        },

        .calculate_statistic = function() {
            # private$.statistic <- ...
        },

        .calculate_p = function() {
            # private$.p_value <- ...
            # when private$.type != "permu"
        },

        .calculate_extra = function() {
            # private$.estimate <- ...
            # private$.ci <- ...
        },

        .calculate_statistic_permu = function() {
            # private$.statistic_permu <- ...
        },

        .calculate_side = function() {
            private$.side <- switch(private$.trend,
                "+" = switch(private$.alternative,
                    greater = "r", less = "l", two_sided = "lr"
                ),
                "-" = switch(private$.alternative,
                    greater = "l", less = "r", two_sided = "lr"
                ),
            )
        },

        .calculate_p_permu = function() {
            private$.p_value <- mean(switch(private$.side,
                l = private$.statistic_permu <= private$.statistic,
                r = private$.statistic_permu >= private$.statistic,
                lr = abs(private$.statistic_permu) >= abs(private$.statistic)
            ))
        },

        .print = function(digits) {
            cat("\n", "\t", private$.name, "\n\n")

            cat(
                paste("scoring:", private$.scoring),
                paste(
                    "type:",
                    if ((type <- private$.type) == "permu") {
                        n_permu <- as.numeric(length(private$.statistic_permu))
                        paste0(type, "(", format(n_permu, digits = digits), ")")
                    } else type
                ),
                paste("method:", private$.method),
                sep = "    "
            )
            cat("\n")

            cat(
                paste(
                    "statistic", "=",
                    format(private$.statistic, digits = digits)
                ),
                {
                    p <- format.pval(private$.p_value, digits = digits)
                    paste(
                        "p-value",
                        if (!startsWith(p, "<")) paste("=", p) else p
                    )
                },
                sep = ", "
            )
            cat("\n")

            cat(
                "alternative hypothesis:",
                if (
                    is.null(private$.param_name) |
                    is.null(private$.null_value)
                ) private$.alternative else {
                    paste(
                        "true", private$.param_name, "is",
                        switch(private$.alternative,
                            two_sided = "not equal to",
                            less = "less than", greater = "greater than"
                        ), private$.null_value
                    )
                }
            )
            cat("\n")

            if (!is.null(private$.estimate)) {
                cat("estimate:", format(private$.estimate, digits = digits))
                cat("\n")
            }

            if (!is.null(private$.ci)) {
                cat(
                    paste0(
                        format(private$.conf_level * 100, digits = digits), "%",
                        " confidence interval: ",
                        "(", format(private$.ci[1], digits = digits), ",",
                        " ", format(private$.ci[2], digits = digits), ")"
                    )
                )
                cat("\n")
            }
        },

        .plot = function(...) {
            do_call(
                func = hist,
                default = list(border = "white"),
                fixed = list(
                    x = private$.statistic_permu,
                    plot = TRUE,
                    xlab = "Statistic",
                    main = "Permutation Distribution"
                ), ...
            )
            abline(v = private$.statistic, lty = "dashed")
        },

        .autoplot = function(...) {
            ggplot2::ggplot() +
                do_call(
                    func = ggplot2::stat_bin,
                    default = list(fill = "gray"),
                    fixed = list(
                        geom = "bar",
                        mapping = ggplot2::aes(x = .data$statistic),
                        data = data.frame(statistic = private$.statistic_permu)
                    ), ...
                ) +
                ggplot2::geom_vline(
                    xintercept = private$.statistic, linetype = "dashed"
                ) +
                ggplot2::labs(
                    title = "Permutation Distribution",
                    x = "Statistic", y = "Frequency"
                ) +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(
                        face = "bold", hjust = 0.5
                    )
                )
        },

        .on_type_change = function() private$.calculate(),
        .on_method_change = function() private$.calculate(),
        .on_scoring_change = function() private$.calculate(),
        .on_null_value_change = function() private$.calculate(),
        .on_conf_level_change = function() private$.calculate_extra(),
        .on_alternative_change = function() {
            private$.calculate_side()
            if (private$.type == "permu") {
                private$.calculate_p_permu()
            } else {
                private$.calculate_p()
            }
        },
        .on_n_permu_change = function() {
            if (private$.type == "permu") {
                private$.calculate()
            }
        }
    ),
    active = list(
        #' @field type The way to calculate the p-value.
        type = function(value) {
            if (missing(value)) {
                private$.type
            } else if (is.null(choices <- formals(self$initialize)$type)) {
                stop(
                    "Can't specify 'type' of a ",
                    "<", class(self)[1], ">", " object"
                )
            } else {
                private$.type <- match.arg(value, eval(choices))
                if (!is.null(private$.raw_data)) {
                    private$.on_type_change()
                }
            }
        },
        #' @field method The method used.
        method = function(value) {
            if (missing(value)) {
                private$.method
            } else if (is.null(choices <- formals(self$initialize)$method)) {
                stop(
                    "Can't specify 'method' of a ",
                    "<", class(self)[1], ">", " object"
                )
            } else {
                private$.method <- match.arg(value, eval(choices))
                if (!is.null(private$.raw_data)) {
                    private$.on_method_change()
                }
            }
        },
        #' @field scoring The scoring system used.
        scoring = function(value) {
            if (missing(value)) {
                private$.scoring
            } else if (is.null(choices <- formals(self$initialize)$scoring)) {
                stop(
                    "Can't specify 'scoring' of a ",
                    "<", class(self)[1], ">", " object"
                )
            } else {
                private$.scoring <- match.arg(value, eval(choices))
                if (!is.null(private$.raw_data)) {
                    private$.on_scoring_change()
                }
            }
        },
        #' @field alternative The alternative hypothesis.
        alternative = function(value) {
            if (missing(value)) {
                private$.alternative
            } else if (is.null(choices <- formals(self$initialize)$alternative)) {
                stop(
                    "Can't specify 'alternative' of a ",
                    "<", class(self)[1], ">", " object"
                )
            } else {
                private$.alternative <- match.arg(value, eval(choices))
                if (!is.null(private$.raw_data)) {
                    private$.on_alternative_change()
                }
            }
        },
        #' @field null_value The true value of the parameter in the null hypothesis.
        null_value = function(value) {
            if (missing(value)) {
                private$.null_value
            } else if (is.null(formals(self$initialize)$null_value)) {
                stop(
                    "Can't specify 'null_value' of a ",
                    "<", class(self)[1], ">", " object"
                )
            } else if (length(value) == 1 & !is.na(value)) {
                private$.null_value <- value
                if (!is.null(private$.raw_data)) {
                    private$.on_null_value_change()
                }
            } else {
                stop("'null_value' must be a single number")
            }
        },
        #' @field conf_level The confidence level of the interval.
        conf_level = function(value) {
            if (missing(value)) {
                private$.conf_level
            } else if (is.null(formals(self$initialize)$conf_level)) {
                stop(
                    "Can't specify 'conf_level' of a ",
                    "<", class(self)[1], ">", " object"
                )
            } else if (
                length(value) == 1 & is.finite(value) & value > 0 & value < 1
            ) {
                private$.conf_level <- value
                if (!is.null(private$.raw_data)) {
                    private$.on_conf_level_change()
                }
            } else {
                stop("'conf_level' must be a single number between 0 and 1")
            }
        },
        #' @field n_permu The number of permutations used.
        n_permu = function(value) {
            if (missing(value)) {
                private$.n_permu
            } else if (is.null(formals(self$initialize)$n_permu)) {
                stop(
                    "Can't specify 'n_permu' of a ",
                    "<", class(self)[1], ">", " object"
                )
            } else if (length(value) == 1 & is.finite(value) & value >= 0) {
                private$.n_permu <- as.integer(value)
                if (!is.null(private$.raw_data)) {
                    private$.on_n_permu_change()
                }
            } else {
                stop("'n_permu' must be a non-negative integer")
            }
        },

        #' @field data The data.
        data = function() private$.raw_data,
        #' @field statistic The test statistic.
        statistic = function() private$.statistic,
        #' @field p_value The p-value.
        p_value = function() private$.p_value,
        #' @field estimate The estimated parameter.
        estimate = function() private$.estimate,
        #' @field ci The confidence interval.
        ci = function() private$.ci
    )
)