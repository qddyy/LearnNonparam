#' @title Signed Score Test
#' 
#' @description Performs two sample signed score test on data vectors. 
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class


SignedScore <- R6Class(
    classname = "Signed Score Test",
    inherit = TwoSamplePairedTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `SignedScore` object. 
        #' 
        #' @param type a character string specifying the way to calculate p-values, must be one of `"permu"` (default) or `"approx"`. 
        #' @param correct a logical indicating whether to apply continuity correction in the normal approximation for the p-value.
        #' @param ranking_method a character string specifying the method of ranking data in computing adjusted signed ranks for tied data, must be one of `"with_zeros"` (default) or `"ignore"`. Note that the data fed will be modified when this parameter is set to "ignore". 
        #' 
        #' @param alternative a character string specifying the alternative hypothesis, must be one of `"two_sided"` (default), `"greater"` or `"less"`.
        #' @param n_permu an integer specifying how many permutations should be used to construct the permutation distribution. If `NULL` (default) then all permutations are used.
        #' @param scoring a character string specifying which scoring system to be used, must be one of `"rank"` (default), `"vw"` or `"savage"`.
        #' 
        #' @return A `SignedScore` object. 
        initialize = function(
            type = c("permu", "approx"), correct = TRUE, ranking_method = c("with_zeros", "ignore"),
            alternative = c("two_sided", "less", "greater"), n_permu = NULL, scoring = c("rank", "vw", "savage")
        ) {
            private$.correct <- correct
            private$.type <- match.arg(type)
            private$.ranking_method <- match.arg(ranking_method)

            super$initialize(alternative = match.arg(alternative), n_permu = n_permu, scoring = match.arg(scoring))
        }
    ),
    private = list(
        .signed_score = NULL,
        .correct = NULL,
        .ranking_method = NULL,

        .calculate = function() {
            private$.calculate_statistic()

            if (private$.type == "permu") {
                private$.permute()
                private$.calculate_statistic_permu()
                private$.calculate_p_permu()
            } else {
                private$.calculate_p()
            }
        },

        .calculate_statistic = function() {
            diff <- private$.data$x - private$.data$y

            if (private$.ranking_method == "ignore") {
                private$.data <- private$.data[diff != 0, ]
                diff <- diff[diff != 0]
            }

            rank <- rank(abs(diff))
            scores <- switch(private$.scoring,
                rank = rank,
                vw = qnorm(rank / (length(rank) + 1)),
                savage = cumsum(1 / length(rank):1)[rank]
            )

            private$.signed_score <- scores * sign(diff)

            private$.statistic <- mean(private$.signed_score)
        },

        .calculate_statistic_permu = function() {
            signed_score <- private$.signed_score
            private$.statistic_permu <- apply(
                private$.swapped_permu, 1,
                function(is_swapped) mean(signed_score * (2 * is_swapped - 1))
            )
        },

        .calculate_p = function() {
            n <- nrow(private$.data)

            SR <- sum(pmax(0, private$.signed_score))
            z <- SR - 1 / 2 * sum(abs(private$.signed_score))
            correction <- if (private$.correct) switch(private$.alternative,
                two_sided = sign(z) * 0.5, greater = 0.5, less = -0.5
            ) else 0
            z <- (z - correction) / sqrt(
                1 / 4 * sum(private$.signed_score^2)
            )

            less <- pnorm(z)
            greater <- pnorm(z, lower.tail = FALSE)
            two_sided <- 2 * min(less, greater)

            private$.p_value <- switch(private$.alternative,
                greater = greater, less = less, two_sided = two_sided
            )
        }
    )
)