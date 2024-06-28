#' @title `r Studentized$private_fields$.name`
#' 
#' @description Performs studentized statistic based multiple comparison on samples.
#' 
#' @aliases multcomp.studentized
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom stats pt ptukey


Studentized <- R6Class(
    classname = "Studentized",
    inherit = MultipleComparison,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `Studentized` object.
        #' 
        #' @template init_params
        #' @param method a character string specifying whether to use Bonferroni's method or Tukey's HSD method.
        #' @param conf_level a number between zero and one indicating the family-wise confidence level to use.
        #' 
        #' @return A `Studentized` object.
        initialize = function(
            type = c("permu", "asymp"),
            method = c("bonferroni", "tukey"),
            scoring = c("none", "rank", "vw", "expon"),
            conf_level = 0.95, n_permu = 1e4
        ) {
            self$type <- type
            self$method <- method
            self$scoring <- scoring
            self$conf_level <- conf_level
            self$n_permu <- n_permu
        }
    ),
    private = list(
        .name = "Multiple Comparison Based on Studentized Statistic",

        .define = function() {
            inverse_lengths <- 1 / tabulate(attr(private$.data, "group"))
            inverse_length_sums <- outer(
                inverse_lengths, inverse_lengths, `+`
            )

            if (private$.scoring == "none") {
                N <- length(private$.data)
                k <- attr(private$.data, "group")[N]
                private$.statistic_func <- function(data, group) {
                    means <- vapply(
                        X = split.default(data, group), FUN = sum,
                        FUN.VALUE = numeric(1), USE.NAMES = FALSE
                    ) * inverse_lengths
                    mse <- sum((data - means[group])^2) / (N - k)
                    function(i, j) {
                        (means[i] - means[j]) / sqrt(
                            mse * inverse_length_sums[i, j]
                        )
                    }
                }
            } else {
                var <- var(private$.data)
                private$.statistic_func <- function(data, group) {
                    means <- vapply(
                        X = split.default(data, group), FUN = sum,
                        FUN.VALUE = numeric(1), USE.NAMES = FALSE
                    ) * inverse_lengths
                    function(i, j) {
                        (means[i] - means[j]) / sqrt(
                            var * inverse_length_sums[i, j]
                        )
                    }
                }
            }
        },

        .calculate_statistic = function() {
            super$.calculate_statistic()

            if (private$.method == "tukey") {
                private$.statistic <- abs(private$.statistic) * sqrt(2)
            }
        },

        .calculate_statistic_permu = function() {
            super$.calculate_statistic_permu()

            if (private$.method == "tukey") {
                private$.statistic <- abs(private$.statistic)

                statistic_permu <- attr(private$.statistic, "permu")
                attr(private$.statistic, "permu") <- apply(
                    X = statistic_permu, MARGIN = 2,
                    FUN = {
                        length_x <- nrow(statistic_permu)
                        function(x) rep.int(max(abs(x)), length_x)
                    }
                )
            }
        },

        .calculate_side = function() {
            private$.side <- switch(private$.method,
                bonferroni = "lr", tukey = "r"
            )
        },

        .calculate_p = function() {
            N <- length(private$.data)
            k <- attr(private$.data, "group")[N]
            df <- if (private$.scoring == "none") N - k else Inf

            private$.p_value <- switch(private$.method,
                bonferroni = get_p_continous(
                    private$.statistic, "t", "lr", df = df
                ),
                tukey = get_p_continous(
                    private$.statistic, "tukey", "r", nmeans = k, df = df
                )
            )
        },

        .calculate_extra = function() {
            if (private$.method == "bonferroni") {
                private$.differ <- (
                    private$.p_value < (
                        (1 - private$.conf_level) / length(private$.p_value)
                    )
                )
            } else {
                super$.calculate_extra()
            }
        }
    )
)