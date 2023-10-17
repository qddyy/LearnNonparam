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
        #' @template init_params
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

        .define = function() {
            diff_positive <- (private$.data$x > private$.data$y)
            private$.statistic_func <- function(swapped) {
                sum(diff_positive != swapped)
            }
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