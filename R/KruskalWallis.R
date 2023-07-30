#' @title Kruskal-Wallis Test
#' 
#' @description Performs k sample Kruskal-Wallis Test on data vectors. 
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class


KruskalWallis <- R6Class(
    classname = "Kruskal-Wallis Test",
    inherit = KSampleTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `KruskalWallis` object. 
        #' 
        #' @param type a character string specifying the way to calculate p-values, must be one of `"permu"` (default) or `"approx"`. 
        #' 
        #' @param n_permu an integer specifying how many permutations should be used to construct the permutation distribution. If `NULL` (default) then all permutations are used.
        #' @param scoring a character string specifying which scoring system to be used, must be one of `"rank"` (default), `"vw"` or `"savage"`.
        #' 
        #' @return A `KruskalWallis` object. 
        initialize = function(
            type = c("permu", "approx"),
            n_permu = NULL, scoring = c("rank", "vw", "savage")
        ) {
            private$.type <- match.arg(type)

            super$initialize(scoring = match.arg(scoring), alternative = "greater", n_permu = n_permu)
        }
    ),
    private = list(
        .calculate_statistic = function() {
            mean <- mean(private$.data)
            var <- var(private$.data)
            private$.statistic_func <- function(data, group) {
                1 / var * sum(tapply(
                    X = data, INDEX = group, FUN = function(x) length(x) * (mean(x) - mean)^2
                ))
            }

            super$.calculate_statistic()
        },

        .calculate_p = function() {
            group <- names(private$.data)
            k <- as.integer(group[length(group)])

            private$.p_value <- pchisq(
                private$.statistic, df = k - 1, , lower.tail = FALSE
            )
        }
    )
)