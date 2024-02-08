#' @title `r PairedDifference$private_fields$.name`
#' 
#' @description Performs differences based paired comparison on data vectors.
#' 
#' @aliases paired.difference
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom stats pnorm


PairedDifference <- R6Class(
    classname = "PairedDifference",
    inherit = TwoSamplePairedTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `PairedDifference` object.
        #' 
        #' @template init_params
        #' @param method a character string specifying the method of ranking data in computing adjusted signed ranks for tied data, must be one of `"with_zeros"` (default) or `"ignore"`. Note that the data will be modified when this parameter is set to `"ignore"`.
        #' @param correct a logical indicating whether to apply continuity correction in the normal approximation for the p-value when `scoring` is set to `"rank"`.
        #' 
        #' @return A `PairedDifference` object.
        initialize = function(
            type = c("permu", "asymp"),
            method = c("with_zeros", "without_zeros"),
            scoring = c("none", "rank", "vw", "expon"),
            alternative = c("two_sided", "less", "greater"),
            n_permu = 0L, correct = TRUE
        ) {
            self$type <- type
            self$method <- method
            self$scoring <- scoring
            self$alternative <- alternative
            self$n_permu <- n_permu
            self$correct <- correct
        }
    ),
    private = list(
        .name = "Paired Comparison Based on Differences",

        .correct = NULL,

        .abs_diff = NULL,

        .define = function() {
            diff <- private$.data$x - private$.data$y

            where_zero <- (diff == 0)

            if (private$.method == "without_zeros") {
                diff <- diff[!where_zero]
                private$.data <- private$.data[!where_zero, ]
            }

            private$.abs_diff <- abs_diff <- if (private$.scoring != "none") {
                score <- get_score(abs(diff), method = private$.scoring)
                if (private$.method == "with_zeros") {
                    `[<-`(score, where_zero, 0)
                } else score
            } else abs(diff)

            positive <- (diff > 0)
            private$.statistic_func <- function(swapped) {
                sum(abs_diff[positive != swapped])
            }
        },

        .calculate_p = function() {
            z <- private$.statistic - sum(private$.abs_diff) / 2
            correction <- if (private$.scoring == "rank" & private$.correct) {
                switch(private$.side, lr = sign(z) * 0.5, r = 0.5, l = -0.5)
            } else 0
            z <- (z - correction) / sqrt(
                sum(private$.abs_diff^2) / 4
            )

            private$.p_value <- get_p_continous(z, "norm", private$.side)
        }
    ),
    active = list(
        #' @field correct Whether to apply continuity correction when `scoring` is set to `"rank"`.
        correct = function(value) {
            if (missing(value)) {
                private$.correct
            } else if (length(value) == 1 & is.logical(value)) {
                private$.correct <- as.logical(value)
                if (
                    !is.null(private$.raw_data) &
                    private$.type == "asymp" & private$.scoring == "rank"
                ) {
                    private$.calculate_p()
                }
            } else {
                stop("'correct' must be a single logical value")
            }
        }
    )
)