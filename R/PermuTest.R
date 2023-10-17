#' @title PermuTest Class
#' 
#' @description This is the abstract base class for permutation test objects. Note that it is not recommended to create objects of this class directly. 
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom ggplot2 ggplot aes stat_bin geom_vline labs theme element_text


PermuTest <- R6Class(
    classname = "PermuTest",
    cloneable = FALSE,
    public = list(
        #' @description Create a new `PermuTest` object. Note that it is not recommended to create objects of this class directly. 
        #' 
        #' @template init_params
        #' 
        #' @return A `PermuTest` object. 
        initialize = function(null_value = 0, alternative = c("two_sided", "less", "greater"), n_permu = NULL, conf_level = 0.95, scoring = c("none", "rank", "vw", "expon")) {
            private$.n_permu <- n_permu
            private$.scoring <- match.arg(scoring)
            private$.null_value <- null_value
            private$.alternative <- match.arg(alternative)
            private$.conf_level <- conf_level
        },

        #' @description Perform test on data. 
        #' 
        #' @param ... the data. 
        #' 
        #' @return The object itself (invisibly).
        test = function(...) {
            private$.input(...)
            private$.check()
            private$.calculate()

            invisible(self)
        },

        #' @description Print the results of the test. 
        #' 
        #' @param digits number of significant digits to be used. 
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
        #' @param ... extra parameters passed to `ggplot2::stat_bin`. 
        #' 
        #' @return The object itself (invisibly). 
        plot = function(...) {
            if (!is.null(private$.raw_data) & private$.type == "permu") {
                private$.plot(...)
            }

            invisible(self)
        }
    ),
    private = list(
        .name = "Permutation Test",
        .param_name = NULL,

        .type = "permu",
        .method = "default",

        .scoring = NULL,

        .n_permu = NULL,

        .raw_data = NULL,
        .data = NULL,

        .statistic_func = NULL,

        .statistic = NULL,
        .statistic_permu = NULL,

        .trend = "+",
        .side = NULL,

        .null_value = NULL,
        .alternative = NULL,
        .p_value = NULL,

        .estimate = NULL,
        .ci = NULL,
        .conf_level = NULL,

        # @Override
        .check = function() {},

        .print = function(digits) {
            cat("\n", "\t", private$.name, "\n\n")

            cat(
                paste("scoring:", private$.scoring),
                paste(
                    "type:",
                    if ((type <- private$.type) == "permu") {
                        n <- as.numeric(length(private$.statistic_permu))
                        paste0(type, "(", format(n, digits = digits), ")")
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
                    paste("p_value", if (!startsWith(p, "<")) paste("=", p) else p)
                },
                sep = ", "
            )
            cat("\n")

            cat(
                "alternative hypothesis:",
                if (is.null(private$.param_name)) private$.alternative else {
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
                    sprintf("%.0f%% confidence interval:", private$.conf_level * 100),
                    paste(format(private$.ci, digits = digits), collapse = " ")
                )
                cat("\n")
            }
        },

        .plot = function(...) {
            histogram <- ggplot() +
                stat_bin(
                    mapping = aes(x = private$.statistic_permu),
                    geom = "bar", fill = "#68aaa1", ...
                ) +
                geom_vline(xintercept = private$.statistic, linetype = "dashed") +
                labs(
                    title = "Permutation Distribution",
                    x = "Statistic", y = "Count"
                ) +
                theme(plot.title = element_text(face = "bold", hjust = 0.5))
            print(histogram)
        },

        # @Override
        .input = function(...) {
            # private$.raw_data <- ...
        },

        # @Override
        .calculate_score = function() {
            # private$.data <- ...
        },

        # @Override
        .define = function() {
            # private$.param_name <- ...
            # private$.statistic_func <- ...
        },

        # @Override
        .calculate_statistic = function() {
            # private$.statistic <- ...
        },

        # @Override
        .calculate_p = function() {
            # private$.p_value <- ...
            # when private$.type != "permu"
        },

        # @Override
        .calculate_extra = function() {
            # private$.estimate <- ...
            # private$.ci <- ...
        },

        # @Override
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
            l <- quote(mean(private$.statistic_permu <= private$.statistic))
            r <- quote(mean(private$.statistic_permu >= private$.statistic))
            lr <- quote(2 * min(eval(l), eval(r)))

            private$.p_value <- eval(get(private$.side))
        },

        .calculate = function() {
            private$.data <- private$.raw_data
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
        }
    ),
    active = list(
        #' @field type The type of the test. 
        type = function(value) {
            if (missing(value)) {
                private$.type
            } else {
                private$.type <- value
                private$.check()
                private$.calculate()
            }
        },
        #' @field method The method used. 
        method = function(value) {
            if (missing(value)) {
                private$.method
            } else {
                private$.method <- value
                private$.check()
                private$.calculate()
            }
        },
        #' @field scoring The scoring system used. 
        scoring = function(value) {
            if (missing(value)) {
                private$.scoring
            } else {
                private$.scoring <- value
                private$.check()
                private$.calculate()
            }
        },
        #' @field null_value The value of the parameter in the null hypothesis. 
        null_value = function(value) {
            if (missing(value)) {
                private$.null_value
            } else {
                private$.null_value <- value
                private$.check()
                private$.calculate()
            }
        },
        #' @field alternative The alternative hypothesis. 
        alternative = function(value) {
            if (missing(value)) {
                private$.alternative
            } else {
                private$.alternative <- value
                private$.check()
                private$.calculate_side()
                if (private$.type == "permu") {
                    private$.calculate_p_permu()
                } else {
                    private$.calculate_p()
                }
            }
        },
        #' @field conf_level The confidence level of the interval. 
        conf_level = function(value) {
            if (missing(value)) {
                private$.conf_level
            } else {
                private$.conf_level <- value
                private$.check()
                private$.calculate_extra()
            }
        },
        #' @field n_permu The number of permutations used. 
        n_permu = function(value) {
            if (missing(value)) {
                private$.n_permu
            } else {
                private$.n_permu <- value
                private$.check()
                if (private$.type == "permu") {
                    private$.calculate()
                }
            }
        },

        #' @field data The data. 
        data = function() private$.data,
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