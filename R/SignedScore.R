#' @title `r SignedScore$private_fields$.name`
#' 
#' @description Performs two sample signed score test on data vectors. 
#' 
#' @aliases paired.signedscore
#' 
#' @export
#' 
#' @importFrom R6 R6Class


SignedScore <- R6Class(
    classname = "SignedScore",
    inherit = TwoSamplePairedTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `SignedScore` object. 
        #' 
        #' @template init_params
        #' @param correct a logical indicating whether to apply continuity correction in the normal approximation for the p-value when `scoring` is set to `"rank"`.
        #' @param ranking_method a character string specifying the method of ranking data in computing adjusted signed ranks for tied data, must be one of `"with_zeros"` (default) or `"ignore"`. Note that the data fed will be modified when this parameter is set to `"ignore"`. 
        #' 
        #' @return A `SignedScore` object. 
        initialize = function(
            type = c("permu", "approx"), correct = TRUE, ranking_method = c("with_zeros", "ignore"),
            alternative = c("two_sided", "less", "greater"), n_permu = NULL, scoring = c("rank", "vw", "expon")
        ) {
            private$.correct <- correct
            private$.type <- match.arg(type)
            private$.ranking_method <- match.arg(ranking_method)

            super$initialize(alternative = match.arg(alternative), n_permu = n_permu, scoring = match.arg(scoring))
        }
    ),
    private = list(
        .name = "Signed Score Test",

        .correct = NULL,
        .ranking_method = NULL,

        .signed_score = NULL,

        .define_statistic = function() {
            diff <- private$.data$x - private$.data$y

            if (private$.ranking_method == "ignore") {
                private$.data <- private$.data[diff != 0, ]
                diff <- diff[diff != 0]
            }

            private$.signed_score <- sign(diff) * score(abs(diff), method = private$.scoring)

            private$.statistic_func <- function(is_swapped, signed_score = private$.signed_score) {
                mean(signed_score * (2 * is_swapped - 1))
            }
        },

        .calculate_p = function() {
            n <- nrow(private$.data)

            sa <- sum(pmax.int(0, private$.signed_score))
            z <- sa - 1 / 2 * sum(abs(private$.signed_score))
            correction <- if (private$.correct) {
                switch(private$.side, lr = sign(z) * 0.5, r = 0.5, l = -0.5)
            } else 0
            z <- (z - correction) / sqrt(
                1 / 4 * sum(private$.signed_score^2)
            )

            private$.p_value <- get_p_continous(z, "norm", private$.side)
        }
    )
)