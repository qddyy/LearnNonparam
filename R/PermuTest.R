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

            private$.side <- switch(private$.trend,
                "+" = switch(private$.alternative,
                    greater = "r", less = "l", two_sided = "lr"
                ),
                "-" = switch(private$.alternative,
                    greater = "l", less = "r", two_sided = "lr"
                ),
            )
        },

        #' @description Feed the data to the test. 
        #' 
        #' @param ... the data. 
        #' 
        #' @return The object itself (invisibly).
        feed = function(...) {
            private$.feed(...)
            private$.check()
            private$.calculate()

            invisible(self)
        },

        #' @description Print the results of the test. Note that it works only if data has been fed. 
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

        .type = "permu",
        .method = "default",

        .scoring = NULL,

        .n_permu = NULL,

        .raw_data = NULL,
        .data = NULL,
        .data_permu = NULL,

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
                {
                    type <- private$.type
                    paste(
                        "type:",
                        if (type == "permu") {
                            paste0(type, "(", format(length(private$.statistic_permu), digits = digits), ")")
                        } else type
                    )
                },
                paste("method:", private$.method),
                sep = "    "
            )
            cat("\n")

            cat(
                paste(
                    "statistic", "=", format(
                        private$.statistic, digits = max(1L, digits - 2L)
                    )
                ),
                {
                    p <- format.pval(private$.p_value, digits = max(1, digits - 2))
                    paste(
                        "p-value", if (startsWith(p, "<")) p else paste("=", p)
                    )
                },
                sep = ", "
            )
            cat("\n")

            cat(
                "alternative hypothesis:",
                if (private$.alternative == "two_sided") "two-sided" else private$.alternative
            )
            cat("\n")

            if (!is.null(private$.estimate)) {
                cat("estimate:", format(private$.estimate, digits = digits))
                cat("\n")
            }

            if (!is.null(private$.ci)) {
                cat(
                    format(100 * private$.conf_level, digits = 2),
                    "percent confidence interval:",
                    paste(
                        format(private$.ci, digits = digits), collapse = " "
                    )
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
        .feed = function(...) {
            # private$.raw_data <- ...
        },

        # @Override
        .calculate_score = function() {
            # private$.data <- ...
        },

        # @Override
        .define_statistic = function() {
            # private$.statistic_func <- ...
        },

        # @Override
        .calculate_statistic = function() {
            # private$.statistic <- ...
        },

        # @Override
        .calculate_p = function() {},

        # @Override
        .calculate_extra = function() {
            # private$.estimate <- ...
            # private$.ci <- ...
        },

        # @Override
        .permute = function() {
            # private$.data_permu <- ...
        },

        # @Override
        .calculate_statistic_permu = function() {
            # private$.statistic_permu <- ...
        },

        .calculate_p_permu = function() {
            r <- quote(mean(private$.statistic_permu >= private$.statistic))
            l <- quote(mean(private$.statistic_permu <= private$.statistic))
            lr <- quote(mean(abs(private$.statistic_permu) >= abs(private$.statistic)))

            private$.p_value <- eval(get(private$.side))
        },

        .calculate = function() {
            private$.data <- private$.raw_data
            if (private$.scoring != "none") {
                private$.calculate_score()
            }

            private$.define_statistic()
            private$.calculate_statistic()

            if (private$.type == "permu") {
                if (!isFALSE(progress <- getOption("pmt_progress"))) {
                    progress <- interactive()
                }

                if (progress) {
                    cat("Permuting...\n")
                    private$.permute()

                    assign(
                        "pb", ProgressBar$new(length(private$.data_permu)),
                        envir = environment(private$.statistic_func)
                    )
                    body(private$.statistic_func) <- as.call(c(
                        as.name("{"),
                        expression(on.exit(pb$update())),
                        body(private$.statistic_func)
                    ))

                    cat("Calculating statistic...\n")
                    private$.calculate_statistic_permu()
                    cat("\n")
                } else {
                    private$.permute()
                    private$.calculate_statistic_permu()
                }

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

        #' @field data Data fed into the object. 
        data = function() private$.data,
        #' @field data_permu All permutations used. 
        data_permu = function() {
            if (private$.type == "permu") {
                private$.data_permu
            }
        },
        #' @field statistic The test statistic. 
        statistic = function() private$.statistic,
        #' @field statistic_permu Test statistics calculated on permutations. 
        statistic_permu = function() {
            if (private$.type == "permu") {
                private$.statistic_permu
            }
        },
        #' @field p_value The p-value. 
        p_value = function() private$.p_value,
        #' @field estimate The estimated parameter. 
        estimate = function() private$.estimate,
        #' @field ci The confidence interval. 
        ci = function() private$.ci
    )
)