#' @title PermuTest Class
#' 
#' @description Abstract class for permutation tests.
#' 
#' @aliases class.pmt
#' 
#' @importFrom R6 R6Class
#' @importFrom compiler cmpfun
#' @importFrom graphics hist abline


PermuTest <- R6Class(
    classname = "PermuTest",
    cloneable = FALSE,
    public = list(
        #' @param ... ignored.
        initialize = function(...) {
            stop("Can't construct an object from abstract class")
        },

        #' @description Perform test on sample(s).
        #' 
        #' @param ... sample(s). Can be numeric vector(s) or a `data.frame` or `list` containing them.
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
            if (is.null(private$.raw_data)) {
                cat(format(self), sep = "\n")
            } else {
                private$.print(digits = digits)
            }

            invisible(self)
        },

        #' @description Plot histogram(s) of the permutation distribution. Note that this method only works if `type` is set to `"permu"`.
        #' 
        #' @template plot_params
        #' @param ... passed to [graphics::hist()] or [ggplot2::stat_bin()].
        #' 
        #' @return The object itself (invisibly).
        plot = function(style = c("graphics", "ggplot2"), ...) {
            if (is.null(private$.raw_data)) {
                warning("Must provide sample(s) before calling the plot method")
            } else if (private$.type != "permu") {
                warning("The plot method only works if type is set to 'permu'")
            } else if (match.arg(style) == "graphics") {
                private$.plot(...)
            } else {
                requireNamespace("ggplot2")
                print(private$.autoplot(...))
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

        .alternative = NULL,
        .link = "+",
        .side = NULL,

        .null_value = NULL,

        .statistic = NULL,
        .statistic_func = NULL,

        .p_value = NULL,

        .estimate = NULL,
        .conf_int = NULL,
        .conf_level = NULL,

        .calculate = function() {
            private$.preprocess()

            if (private$.scoring != "none") {
                private$.calculate_score()
            }

            private$.define()

            private$.calculate_side()
            if (private$.type == "permu") {
                private$.compile_statistic_closure()
                private$.calculate_statistic_permu()
                private$.calculate_n_permu()
                private$.calculate_p_permu()
            } else {
                private$.calculate_statistic()
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

        .compile_statistic_closure = function() {
            statistic_closure <- cmpfun(private$.statistic_func)
            private$.statistic_func <- function(...) statistic_closure
        },

        .calculate_statistic_permu = function() {
            # private$.statistic <- ... (with attr "permu")
        },

        .calculate_n_permu = function() {
            attr(private$.n_permu, "n_used") <- length(
                attr(private$.statistic, "permu")
            )
        },

        .calculate_side = function() {
            private$.side <- switch(private$.link,
                "+" = switch(private$.alternative,
                    greater = "r", less = "l", two_sided = "lr"
                ),
                "-" = switch(private$.alternative,
                    greater = "l", less = "r", two_sided = "lr"
                ),
            )
        },

        .calculate_p = function() {
            # private$.p_value <- ...
        },

        .calculate_p_permu = function() {
            statistic_permu <- attr(private$.statistic, "permu")

            delayedAssign(
                "l", sum(
                    statistic_permu <= private$.statistic
                ) / length(statistic_permu)
            )
            delayedAssign(
                "r", sum(
                    statistic_permu >= private$.statistic
                ) / length(statistic_permu)
            )
            delayedAssign(
                "lr", 2 * min(l, r, 0.5)
            )

            private$.p_value <- eval(as.name(private$.side))
        },

        .calculate_extra = function() {
            # private$.estimate <- ...
            # private$.conf_int <- ...
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
            private$.calculate_statistic_permu()
            private$.calculate_n_permu()
            private$.calculate_p_permu()
        },

        .print = function(digits) {
            cat("\n", "\t", private$.name, "\n\n")

            cat(
                paste("scoring:", private$.scoring),
                paste(
                    "type:",
                    if ((type <- private$.type) == "permu") {
                        n_used <- as.numeric(attr(private$.n_permu, "n_used"))
                        paste0(type, "(", format(n_used, digits = digits), ")")
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

            if (!is.null(private$.alternative)) {
                cat(
                    "alternative hypothesis:",
                    if (
                        !is.null(private$.param_name) &&
                        !is.null(private$.null_value)
                    ) {
                        paste(
                            "true", private$.param_name, "is",
                            switch(private$.alternative,
                                two_sided = "not equal to",
                                less = "less than", greater = "greater than"
                            ), private$.null_value
                        )
                    } else private$.alternative
                )
                cat("\n")
            }

            if (!is.null(private$.estimate)) {
                cat("estimate:", format(private$.estimate, digits = digits))
                cat("\n")
            }

            if (!is.null(private$.conf_int)) {
                cat(
                    paste0(
                        format(private$.conf_level * 100, digits = digits), "%",
                        " confidence interval: ",
                        "(", format(private$.conf_int[1], digits = digits), ",",
                        " ", format(private$.conf_int[2], digits = digits), ")"
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
                    x = attr(private$.statistic, "permu"),
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
                        data = data.frame(
                            statistic = attr(private$.statistic, "permu")
                        )
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
        #' @field null_value The hypothesized value of the parameter in the null hypothesis.
        null_value = function(value) {
            if (missing(value)) {
                private$.null_value
            } else if (is.null(formals(self$initialize)$null_value)) {
                stop(
                    "Can't specify 'null_value' of a ",
                    "<", class(self)[1], ">", " object"
                )
            } else if (length(value) == 1 && !is.na(value)) {
                private$.null_value <- as.numeric(value)
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
                length(value) == 1 && is.finite(value) && value > 0 && value < 1
            ) {
                private$.conf_level <- as.numeric(value)
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
            } else if (length(value) == 1 && is.finite(value) && value >= 0) {
                private$.n_permu <- ceiling(value)
                if (!is.null(private$.raw_data) && private$.type == "permu") {
                    private$.on_n_permu_change()
                }
            } else {
                stop("'n_permu' must be a non-negative integer")
            }
        },

        #' @field data The data.
        data = function() private$.raw_data,
        #' @field statistic The test statistic.
        statistic = function() `attr<-`(private$.statistic, "permu", NULL),
        #' @field p_value The p-value.
        p_value = function() private$.p_value,
        #' @field estimate The estimated value of the parameter.
        estimate = function() private$.estimate,
        #' @field conf_int The confidence interval of the parameter.
        conf_int = function() private$.conf_int
    )
)