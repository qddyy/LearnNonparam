#' @title `r Sign$private_fields$.name`
#' 
#' @description Performs two sample sign test on data vectors. 
#' 
#' @aliases paired.sign
#' 
#' @export
#' 
#' @importFrom R6 R6Class


Sign <- R6Class(
    classname = "Sign",
    inherit = TwoSamplePairedTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `Sign` object. 
        #' 
        #' @param type a character string specifying the way to calculate p-values, must be one of `"permu"` (default), `"approx"` or `"exact"`. 
        #' @param correct a logical indicating whether to apply continuity correction in the normal approximation for the p-value.
        #' 
        #' @param alternative a character string specifying the alternative hypothesis, must be one of `"two_sided"` (default), `"greater"` or `"less"`.
        #' @param n_permu an integer specifying how many permutations should be used to construct the permutation distribution. If `NULL` (default) then all permutations are used.
        #' 
        #' @return A `Sign` object. 
        initialize = function(
            type = c("permu", "approx", "exact"), correct = TRUE,
            alternative = c("two_sided", "less", "greater"), n_permu = NULL
        ) {
            private$.correct <- correct
            private$.type <- match.arg(type)

            super$initialize(alternative = match.arg(alternative), n_permu = n_permu)
        }
    ),
    private = list(
        .name = "Sign Test",

        .correct = NULL,

        .sign = NULL,

        .calculate_statistic = function() {
            private$.sign <- sign(private$.data$x - private$.data$y)

            private$.statistic <- sum(private$.sign == 1)
        },

        .calculate_statistic_permu = function() {
            private$.statistic_permu <- apply(
                X = private$.swapped_permu, MARGIN = 1,
                FUN = function(is_swapped, sign) {
                    sum(sign * (2 * is_swapped - 1) == 1)
                }, sign = private$.sign
            )
        },

        .calculate_p = function() {
            n <- nrow(private$.data)

            if (private$.type == "exact") {
                private$.p_value <- get_p_binom(private$.statistic, n, 0.5, private$.side)
            }

            if (private$.type == "approx") {
                z <- private$.statistic - n / 2
                correction <- if (private$.correct) {
                    switch(private$.alternative,
                        two_sided = sign(z) * 0.5, greater = 0.5, less = -0.5
                    )
                } else 0
                z <- (z - correction) / sqrt(n / 4)

                private$.p_value <- get_p_continous(z, "norm", private$.side)
            }
        }
    )
)