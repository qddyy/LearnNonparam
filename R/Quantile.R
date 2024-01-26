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
            self$type <- type
            self$alternative <- alternative
            self$null_value <- null_value
            self$conf_level <- conf_level
            self$prob <- prob
            self$correct <- correct
        }
    ),
    private = list(
        .name = "Quantile Test",

        .prob = NULL,
        .correct = NULL,

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
        },

        .on_null_value_change = function() {
            private$.calculate_statistic()
            private$.calculate_p()
        }
    ),
    active = list(
        #' @field prob The probability.
        prob = function(value) {
            if (missing(value)) {
                private$.prob
            } else if (
                length(value) == 1 & is.finite(value) & value >= 0 & value <= 1
            ) {
                private$.prob <- value
                if (!is.null(private$.raw_data)) {
                    private$.define()
                    private$.calculate_p()
                    private$.calculate_extra()
                }
            } else {
                stop_without_call("'prob' must be a single number between 0 and 1")
            }
        },
        #' @template active_params
        correct = function(value) {
            if (missing(value)) {
                private$.correct
            } else if (length(value) == 1 & is.logical(value)) {
                private$.correct <- value
                if (!is.null(private$.raw_data) & private$.type == "asymp") {
                    private$.calculate_p()
                }
            } else {
                stop_without_call("'correct' must be a single logical value")
            }
        }
    )
)