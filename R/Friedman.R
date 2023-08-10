#' @title `r Friedman$private_fields$.name`
#' 
#' @description Performs Friedman test on data for a randomized complete block design. 
#' 
#' @aliases rcbd.friedman
#' 
#' @export
#' 
#' @importFrom R6 R6Class


Friedman <- R6Class(
    classname = "Friedman",
    inherit = RCBD,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `Friedman` object. 
        #' 
        #' @param type a character string specifying the way to calculate p-values, must be one of `"permu"` (default) or `"approx"`. 
        #' 
        #' @param n_permu an integer specifying how many permutations should be used to construct the permutation distribution. If `NULL` (default) then all permutations are used.
        #' 
        #' @return A `Friedman` object. 
        initialize = function(
            type = c("permu", "approx"),
            n_permu = NULL
        ) {
            private$.type <- match.arg(type)

            super$initialize(scoring = "rank", alternative = "greater", n_permu = n_permu)
        }
    ),
    private = list(
        .name = "Friedman Test",

        .calculate_statistic = function() {
            private$.statistic_func <- switch(private$.type,
                permu = function(df) sum(rowMeans(df)^2),
                approx = function(df) {
                    ncol(df)^2 / sum(vapply(df, var, numeric(1))) *
                    sum((rowMeans(df) - (nrow(df) + 1) / 2)^2)
                }
            )

            super$.calculate_statistic()
        },

        .calculate_p = function() {
            k <- nrow(private$.data)

            private$.p_value <- pchisq(private$.statistic, df = k - 1, lower.tail = FALSE)
        }
    )
)
