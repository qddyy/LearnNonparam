#' @title `r Studentized$private_fields$.name`
#' 
#' @description Performs studentized statistic based multiple comparison on samples.
#' 
#' @aliases multcomp.studentized
#' 
#' @examples
#' t <- pmt(
#'     "multcomp.studentized", method = "bonferroni"
#' )$test(Table3.3.1)$print()
#' 
#' t$type <- "asymp"
#' t
#' 
#' t$scoring <- "rank"
#' t
#' 
#' t$method <- "tukey"
#' t
#' 
#' t$scoring <- "none"
#' t
#' 
#' t$type <- "permu"
#' t
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom stats pt ptukey sd


Studentized <- R6Class(
    classname = "Studentized",
    inherit = MultipleComparison,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `Studentized` object.
        #' 
        #' @template pmt_init_params
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
            private$.statistic_func <- function(data, group) {
                inv_lengths <- 1 / tabulate(group)

                weights <- 1 / sqrt(outer(inv_lengths, inv_lengths, `+`))

                if (private$.scoring == "none") {
                    N <- length(data)
                    k <- group[N]

                    function(data, group) {
                        means <- rowsum.default(data, group) * inv_lengths

                        weights <- weights / sqrt(
                            sum((data - means[group])^2) / (N - k)
                        )

                        function(i, j) (means[i] - means[j]) * weights[i, j]
                    }
                } else {
                    weights <- weights / sd(data)

                    function(data, group) {
                        means <- rowsum.default(data, group) * inv_lengths

                        function(i, j) (means[i] - means[j]) * weights[i, j]
                    }
                }
            }
        },

        .calculate_statistic = function() {
            super$.calculate_statistic()

            if (private$.method == "tukey") {
                private$.statistic <- abs(private$.statistic) * sqrt(2)

                if (private$.type == "permu") {
                    attr(private$.statistic, "permu") <- apply(
                        X = attr(private$.statistic, "permu"),
                        MARGIN = 2, FUN = {
                            length_x <- length(private$.statistic)
                            function(x) rep.int(max(abs(x)), length_x)
                        }
                    ) * sqrt(2)
                }
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