#' @title `r Sign$private_fields$.name`
#' 
#' @description Performs two sample sign test on data vectors.
#' 
#' @aliases paired.sign
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom stats pnorm


Sign <- R6Class(
    classname = "Sign",
    inherit = TwoSamplePairedTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `Sign` object.
        #' 
        #' @template init_params
        #' 
        #' @return A `Sign` object.
        initialize = function(
            type = c("permu", "asymp", "exact"),
            alternative = c("two_sided", "less", "greater"),
            n_permu = 0L, correct = TRUE
        ) {
            self$type <- type
            self$alternative <- alternative
            self$n_permu <- n_permu
            self$correct <- correct
        }
    ),
    private = list(
        .name = "Two Sample Sign Test",

        .correct = NULL,

        .define = function() {
            private$.statistic_func <- function(x, y) sum(x > y)
        },

        .calculate_p = function() {
            n <- nrow(private$.data)

            if (private$.type == "exact") {
                private$.p_value <- get_p_binom(
                    private$.statistic, n, 0.5, private$.side
                )
            }

            if (private$.type == "asymp") {
                z <- private$.statistic - n / 2
                correction <- if (private$.correct) {
                    switch(private$.side, lr = sign(z) * 0.5, r = 0.5, l = -0.5)
                } else 0
                z <- (z - correction) / sqrt(n / 4)

                private$.p_value <- get_p_continous(z, "norm", private$.side)
            }
        }
    ),
    active = list(
        #' @template active_params
        correct = function(value) {
            if (missing(value)) {
                private$.correct
            } else if (length(value) == 1 & is.logical(value)) {
                private$.correct <- as.logical(value)
                if (!is.null(private$.raw_data) & private$.type == "asymp") {
                    private$.calculate_p()
                }
            } else {
                stop("'correct' must be a single logical value")
            }
        }
    )
)