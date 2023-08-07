#' @title Tukey's HSD
#' 
#' @description Performs Tukey's honest significant difference method on data vectors. Note that procedure modified by Kramer is performed for unequal sample sizes. 
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class


TukeyHSD <- R6Class(
    classname = "Tukey HSD",
    inherit = MultipleComparison,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `TukeyHSD` object. 
        #' 
        #' @param type a character string specifying the way to calculate p-values, must be one of `"permu"` (default) or `"approx"`. 
        #' 
        #' @param conf_level a numeric value between zero and one giving the family-wise confidence level to use. 
        #' @param n_permu an integer specifying how many permutations should be used to construct the permutation distribution. If `NULL` (default) then all permutations are used. 
        #' @param scoring a character string specifying which scoring system to be used, must be one of `"none"` (default), `"rank`, `"vw"` or `"expon"`. 
        #' 
        #' @return A `TukeyHSD` object. 
        initialize = function(
            type = c("permu", "approx"),
            conf_level = 0.95, n_permu = NULL, scoring = c("none", "rank", "vw", "expon")
        ) {
            private$.type <- match.arg(type)
            
            super$initialize(conf_level = conf_level, n_permu = n_permu, scoring = match.arg(scoring))
        }
    ),
    private = list(
        .calculate_statistic = function() {
            if (private$.scoring == "none") {
                N <- length(private$.data)
                k <- as.integer(names(private$.data)[N])
                private$.statistic_func <- function(x, y, data) {
                    mse <- sum(tapply(
                        data, names(data), function(x) (length(x) - 1) * var(x)
                    )) / (N - k)
                    (mean(x) - mean(y)) / sqrt(
                        mse / 2 * (1 / length(x) + 1 / length(y))
                    )
                }
            } else {
                var <- var(private$.data)
                private$.statistic_func <- function(x, y, data) {
                    (mean(x) - mean(y)) / sqrt(
                        var / 2 * (1 / length(x) + 1 / length(y))
                    )
                }
            }

            super$.calculate_statistic()
        },

        .calculate_p_permu = function() {
            Q <- apply(abs(private$.statistic_permu), 2, max)

            private$.p_value <- vapply(
                X = abs(private$.statistic), FUN.VALUE = numeric(1),
                FUN = function(T_abs) mean(Q >= T_abs)
            )
        },

        .calculate_p = function() {
            N <- length(private$.data)
            k <- as.integer(names(private$.data)[N])
            df <- if (private$.scoring == "none") N - k else Inf

            private$.p_value <- ptukey(
                abs(private$.statistic), nmeans = k, df = df, lower.tail = FALSE
            )
        }
    )
)