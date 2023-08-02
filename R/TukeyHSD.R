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
        #' @param signif_level a numeric value between zero and one giving the significance level.
        #' @param n_permu an integer specifying how many permutations should be used to construct the permutation distribution. If `NULL` (default) then all permutations are used.
        #' @param scoring a character string specifying which scoring system to be used, must be one of `"none"` (default), `"rank`, `"vw"` or `"expon"`.
        #' 
        #' @return A `TukeyHSD` object. 
        initialize = function(
            type = c("permu", "approx"),
            signif_level = 0.05, n_permu = NULL, scoring = c("none", "rank", "vw", "expon")
        ) {
            private$.type <- match.arg(type)
            
            super$initialize(signif_level = signif_level, n_permu = n_permu, scoring = match.arg(scoring))
        }
    ),
    private = list(
        .calculate_statistic = function() {
            N <- length(private$.data)
            k <- length(unique(names(private$.data)))
            private$.statistic_func <- function(x, y, data) {
                MSE <- sum(tapply(
                    data, names(data), function(x) (length(x) - 1) * var(x)
                )) / (N - k)

                (mean(x) - mean(y)) / sqrt(
                    MSE / 2 * (1 / length(x) + 1 / length(y))
                )
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
            k <- length(unique(names(private$.data)))

            if (private$.scoring == "none") {
                N <- length(private$.data)

                private$.p_value <- ptukey(
                    abs(private$.statistic), nmeans = k, df = N - k, lower.tail = FALSE
                )
            } else {
                private$.p_value <- ptukey(
                    abs(private$.statistic), nmeans = k, df = Inf, lower.tail = FALSE
                )
            }
        }
    )
)