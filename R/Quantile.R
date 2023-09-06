#' @title `r Quantile$private_fields$.name`
#' 
#' @description Performs one sample quantile test on data vectors. In addition, a confidence interval will be calculated. 
#' 
#' @aliases onesample.quantile
#' 
#' @export
#' 
#' @importFrom R6 R6Class


Quantile <- R6Class(
    classname = "Quantile",
    inherit = OneSampleTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `Quantile` object. 
        #' 
        #' @template init_params
        #' @param prob a numeric between 0 and 1 indicating the probability. 
        #' 
        #' @return A `Quantile` object. 
        initialize = function(
            type = c("approx", "exact"), correct = TRUE, prob = 0.5,
            null_value = 0, alternative = c("two_sided", "less", "greater"), conf_level = 0.95
        ) {
            private$.prob <- prob
            private$.correct <- correct
            private$.type <- match.arg(type)

            super$initialize(null_value = null_value, alternative = match.arg(alternative), conf_level = conf_level)
        }
    ),
    private = list(
        .name = "Quantile Test",

        .prob = NULL,
        .correct = NULL,

        .calculate_statistic = function() {
            private$.statistic <- sum(private$.data > private$.null_value)
        },

        .calculate_p = function() {
            n <- length(private$.data)
            p <- private$.prob

            if (private$.type == "exact") {
                private$.p_value <- get_p_binom(
                    private$.statistic, n, p, private$.side
                )
            }

            if (private$.type == "approx") {
                z <- private$.statistic - n * p
                correction <- if (private$.correct) {
                    switch(private$.alternative,
                        two_sided = sign(z) * 0.5, greater = 0.5, less = -0.5
                    )
                } else 0
                z <- (z - correction) / sqrt(n * p * (1 - p))

                private$.p_value <- get_p_continous(z, "norm", private$.side)
            }
        },

        .calculate_extra = function() {
            n <- length(private$.data)
            beta <- 1 - (1 - private$.conf_level) / 2
            p <- private$.prob
            
            d <- qnorm(beta) * sqrt(n * p * (1 - p))
            a <- round(p * n - d)
            b <- round(p * n + 1 + d)

            y <- sort(private$.data)

            private$.ci <- if (a >= 1 & b <= n) c(y[a], y[b]) else c(NA, NA)
        }
    ),
    active = list(
        #' @field prob The probability. 
        prob = function(value) {
            if (missing(value)) {
                private$.prob
            } else {
                private$.prob <- value
                private$.check()
                private$.calculate()
            }
        }
    )
)