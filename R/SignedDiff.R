#' @title `r SignedDiff$private_fields$.name`
#' 
#' @description Performs two sample signed score test on data vectors. 
#' 
#' @aliases paired.signeddiff
#' 
#' @export
#' 
#' @importFrom R6 R6Class


SignedDiff <- R6Class(
    classname = "SignedDiff",
    inherit = TwoSamplePairedTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `SignedDiff` object. 
        #' 
        #' @template init_params
        #' @param scoring a character string specifying which scoring system to be used on the absolute differences. 
        #' @param method a character string specifying the method of ranking data in computing adjusted signed ranks for tied data, must be one of `"with_zeros"` (default) or `"ignore"`. Note that the data will be modified when this parameter is set to `"ignore"`. 
        #' @param correct a logical indicating whether to apply continuity correction in the normal approximation for the p-value when `scoring` is set to `"rank"`.
        #' 
        #' @return A `SignedDiff` object. 
        initialize = function(
            type = c("permu", "approx"), method = c("with_zeros", "ignore"), correct = TRUE,
            alternative = c("two_sided", "less", "greater"), n_permu = 0L, scoring = c("none", "rank", "vw", "expon")
        ) {
            private$.correct <- correct
            private$.type <- match.arg(type)
            private$.method <- match.arg(method)

            super$initialize(alternative = match.arg(alternative), n_permu = n_permu, scoring = match.arg(scoring))
        }
    ),
    private = list(
        .name = "Paired Comparison Based on Signed Differences",

        .correct = NULL,

        .abs_diff = NULL,

        .define = function() {
            diff <- private$.data$x - private$.data$y

            if (private$.method == "ignore") {
                private$.data <- private$.data[diff != 0, ]
                diff <- diff[diff != 0]
            }

            private$.abs_diff <- abs_diff <- abs(diff)
            if (private$.scoring != "none") {
                private$.abs_diff <- get_score(
                    private$.abs_diff, method = private$.scoring
                )
            }

            diff_positive <- (diff > 0)
            private$.statistic_func <- function(swapped) {
                sum(abs_diff[diff_positive != swapped])
            }
        },

        .calculate_p = function() {
            n <- nrow(private$.data)

            z <- private$.statistic - sum(private$.abs_diff) / 2
            correction <- if (private$.scoring == "rank" & private$.correct) {
                switch(private$.side, lr = sign(z) * 0.5, r = 0.5, l = -0.5)
            } else 0
            z <- (z - correction) / sqrt(
                sum(private$.abs_diff^2) / 4
            )

            private$.p_value <- get_p_continous(z, "norm", private$.side)
        }
    )
)