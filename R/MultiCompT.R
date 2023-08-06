#' @title Multiple Comparison Based on t Statistic
#' 
#' @description Performs t statistic based multiple comparison on data vectors. 
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class


MultiCompT <- R6Class(
    classname = "Multiple Comparison (t test)",
    inherit = MultipleComparison,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `MultiCompT` object. 
        #' 
        #' @param type a character string specifying the way to calculate p-values, must be one of `"permu"` (default) or `"approx"`. 
        #' @param bonferroni a logical indicating whether to apply bonferroni adjustment. 
        #' 
        #' @param conf_level a numeric value between zero and one giving the family-wise confidence level to use. 
        #' @param n_permu an integer specifying how many permutations should be used to construct the permutation distribution. If `NULL` (default) then all permutations are used.
        #' @param scoring a character string specifying which scoring system to be used, must be one of `"none"` (default), `"rank`, `"vw"` or `"expon"`.
        #' 
        #' @return A `MultiCompT` object. 
        initialize = function(
            type = c("permu", "approx"), bonferroni = TRUE,
            conf_level = 0.95, n_permu = NULL, scoring = c("none", "rank", "vw", "expon")
        ) {
            private$.type <- match.arg(type)
            private$.bonferroni <- bonferroni
            
            super$initialize(conf_level = conf_level, n_permu = n_permu, scoring = match.arg(scoring))
        }
    ),
    private = list(
        .bonferroni = NULL,

        .calculate_statistic = function() {
            if (private$.scoring == "none") {
                N <- length(private$.data)
                k <- as.integer(names(private$.data)[N])
                private$.statistic_func <- function(x, y, data) {
                    mse <- sum(tapply(
                        data, names(data), function(x) (length(x) - 1) * var(x)
                    )) / (N - k)
                    (mean(x) - mean(y)) / sqrt(
                        mse * (1 / length(x) + 1 / length(y))
                    )
                }
            } else {
                var <- var(private$.data)
                private$.statistic_func <- function(x, y, data) {
                    (mean(x) - mean(y)) / sqrt(
                        var * (1 / length(x) + 1 / length(y))
                    )
                }
            }

            super$.calculate_statistic()
        },

        .calculate_p = function() {
            N <- length(private$.data)
            k <- as.integer(names(private$.data)[N])
            df <- if (private$.scoring == "none") N - k else Inf

            private$.p_value <- 2 * pt(
                abs(private$.statistic), df = df, lower.tail = FALSE
            )
        },

        .calculate_extra = function() {
            super$.calculate_extra()

            if (private$.bonferroni) {
                private$.multicomp$differ <- (
                    private$.p_value < (1 - private$.conf_level) / nrow(private$.multicomp)
                )
            }
        }
    )
)