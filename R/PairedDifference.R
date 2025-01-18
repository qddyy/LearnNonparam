#' @title `r PairedDifference$private_fields$.name`
#' 
#' @description Performs differences based paired comparison on samples.
#' 
#' @aliases paired.difference
#' 
#' @examples
#' pmt(
#'     "paired.difference",
#'     alternative = "greater", scoring = "none", n_permu = 0
#' )$test(Table4.1.1)$print()
#' 
#' pmt(
#'     "paired.difference", n_permu = 0
#' )$test(Table4.1.3)$print()
#' 
#' t <- pmt(
#'     "paired.difference", scoring = "rank",
#'     alternative = "greater", n_permu = 0
#' )$test(Table4.1.1)$print()
#' 
#' t$type <- "asymp"
#' t
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
        #' @template pmt_init_params
        #' @template location_init_params
        #' @param method a character string specifying the method of ranking data in computing adjusted signed scores for tied data, must be one of `"with_zeros"` (default) or `"without_zeros"`.
        #' @param correct a logical indicating whether to apply continuity correction in the normal approximation for the p-value when `scoring` is set to `"rank"`.
        #' 
        #' @return A `PairedDifference` object.
        initialize = function(
            type = c("permu", "asymp"),
            method = c("with_zeros", "without_zeros"),
            scoring = c("none", "rank", "vw", "expon"),
            alternative = c("two_sided", "less", "greater"),
            null_value = 0, n_permu = 1e4, correct = TRUE
        ) {
            self$type <- type
            self$method <- method
            self$scoring <- scoring
            self$alternative <- alternative
            self$null_value <- null_value
            self$n_permu <- n_permu
            self$correct <- correct
        }
    ),
    private = list(
        .name = "Paired Comparison Based on Differences",

        .correct = NULL,

        .define = function() {
            private$.data$x <- private$.data$x - private$.null_value

            private$.data$x <- abs(private$.data$x - private$.data$y)
            private$.data$y <- 0

            if (private$.method == "without_zeros") {
                private$.data <- private$.data[private$.data$x != 0, ]
            }

            private$.statistic_func <- function(...) function(x, y) sum(x)
        },

        .calculate_score = function() {
            score <- get_score(private$.data$x, private$.scoring)

            private$.data$x <- if (private$.method == "with_zeros") {
                `[<-`(score, private$.data$x == 0, 0)
            } else score
        },

        .calculate_p = function() {
            z <- private$.statistic - sum(private$.data$x) / 2
            correction <- if (private$.scoring == "rank" && private$.correct) {
                switch(private$.side, lr = sign(z) * 0.5, r = 0.5, l = -0.5)
            } else 0
            z <- (z - correction) / sqrt(
                sum(private$.data$x^2) / 4
            )

            private$.p_value <- get_p_continous(z, "norm", private$.side)
        }
    ),
    active = list(
        #' @field correct Whether to apply continuity correction when `scoring` is set to `"rank"`.
        correct = function(value) {
            if (missing(value)) {
                private$.correct
            } else if (length(value) == 1 && is.logical(value)) {
                private$.correct <- as.logical(value)
                if (
                    !is.null(private$.raw_data) &&
                    private$.type == "asymp" && private$.scoring == "rank"
                ) {
                    private$.calculate_p()
                }
            } else {
                stop("'correct' must be a single logical value")
            }
        }
    )
)