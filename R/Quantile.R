#' @title `r Quantile$private_fields$.name`
#' 
#' @description Performs one sample quantile test on data vectors. In addition, a confidence interval will be calculated.
#' 
#' @aliases onesample.quantile
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom stats pnorm qnorm


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
            type = c("asymp", "exact"), 
            alternative = c("two_sided", "less", "greater"),
            null_value = 0, conf_level = 0.95,
            prob = 0.5, correct = TRUE
        ) {
            private$.init(
                type = type, alternative = alternative,
                null_value = null_value, conf_level = conf_level,
                prob = prob, correct = correct
            )
        }
    ),
    private = list(
        .name = "Quantile Test",

        .prob = NULL,
        .correct = NULL,

        .init = function(prob, correct, ...) {
            super$.init(...)

            if (!missing(prob)) {
                if (
                    length(prob) == 1 & is.finite(prob) &
                    prob >= 0 & prob <= 1
                ) {
                    private$.prob <- prob
                } else {
                    stop("'prob' must be a single number between 0 and 1")
                }
            }

            if (!missing(correct)) {
                if (length(correct) == 1 & is.logical(correct)) {
                    private$.correct <- correct
                } else {
                    stop("'correct' must be a single logical value")
                }
            }
        },

        .define = function() {
            private$.param_name <- paste(private$.prob, "quantile")
        },

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

            if (private$.type == "asymp") {
                z <- private$.statistic - n * p
                correction <- if (private$.correct) {
                    switch(private$.side, lr = sign(z) * 0.5, r = 0.5, l = -0.5)
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

            private$.ci <- c(
                if (a >= 1) y[a] else -Inf,
                if (b <= n) y[b] else Inf
            )
        }
    ),
    active = list(
        #' @field null_value The true quantile in the null hypothesis.
        null_value = function(value) {
            if (missing(value)) {
                private$.null_value
            } else {
                private$.init(null_value = value)
                if (!is.null(private$.raw_data)) {
                    private$.calculate_statistic()
                    private$.calculate_p()
                }
            }
        },
        #' @field prob The probability.
        prob = function(value) {
            if (missing(value)) {
                private$.prob
            } else {
                private$.init(prob = value)
                if (!is.null(private$.raw_data)) {
                    private$.define()
                    private$.calculate_p()
                    private$.calculate_extra()
                }
            }
        },
        #' @template active_params
        correct = function(value) {
            if (missing(value)) {
                private$.correct
            } else {
                private$.init(correct = value)
                if (!is.null(private$.raw_data) & private$.type == "asymp") {
                    private$.calculate_p()
                }
            }
        }
    )
)