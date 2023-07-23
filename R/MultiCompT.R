#' @title Multiple Comparison Based on T Test
#' 
#' @description Performs t test based multiple comparison on data vectors. 
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
        #' @param signif_level a numeric value between zero and one giving the significance level.
        #' @param n_permu an integer specifying how many permutations should be used to construct the permutation distribution. If `NULL` (default) then all permutations are used.
        #' @param scoring a character string specifying which scoring system to be used, must be one of `"none"` (default), `"rank`, `"vw"` or `"savage"`.
        #' 
        #' @return A `MultiCompT` object. 
        initialize = function(
            type = c("permu", "approx"), bonferroni = TRUE,
            signif_level = 0.05, n_permu = NULL, scoring = c("none", "rank", "vw", "savage")
        ) {
            private$.type <- match.arg(type)
            private$.bonferroni <- bonferroni
            
            super$initialize(signif_level = signif_level, n_permu = n_permu, scoring = match.arg(scoring))
        }
    ),
    private = list(
        .bonferroni = NULL,

        .calculate_statistic = function() {
            if (private$.scoring == "none") {
                N <- length(private$.data)
                k <- length(unique(names(private$.data)))
                private$.statistic_func <- function(x, y, data) {
                    MSE <- sum(tapply(
                        data, names(data), function(x) (length(x) - 1) * var(x)
                    )) / (N - k)

                    (mean(x) - mean(y)) / sqrt(
                        MSE * (1 / length(x) + 1 / length(y))
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
            if (private$.scoring == "none") {
                N <- length(private$.data)
                k <- length(unique(names(private$.data)))

                private$.p_value <- 2 * pt(
                    abs(private$.statistic), df = N - k, lower.tail = FALSE
                )
            } else {
                private$.p_value <- 2 * pnorm(
                    abs(private$.statistic), lower.tail = FALSE
                )
            }
        },

        .prepare_multicomp = function() {
            super$.prepare_multicomp()

            if (private$.bonferroni) {
                private$.multicomp$differ <- private$.p_value < (
                    private$.signif_level / nrow(private$.multicomp)
                )
            }
        }
    ),
    active = list(
        #' @field bonferroni Whether to apply bonferroni adjustment. 
        bonferroni = function(value) {
            if (missing(value)) {
                private$.bonferroni
            } else {
                private$.bonferroni <- value
                private$.check()
                private$.prepare_multicomp()
            }
        } 
    )
)