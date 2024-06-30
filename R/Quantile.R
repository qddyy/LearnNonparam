#' @title `r Quantile$private_fields$.name`
#' 
#' @description Performs quantile test on a single sample. In addition, an estimation and a confidence interval for the desired quantile will be calculated.
#' 
#' @aliases onesample.quantile
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom stats pnorm qnorm quantile


Quantile <- R6Class(
    classname = "Quantile",
    inherit = OneSampleTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `Quantile` object.
        #' 
        #' @template pmt_init_params
        #' @param null_value a number indicating the hypothesized value of the quantile.
        #' @param prob a number between zero and one indicating the probability associated with the quantile.
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
            p <- private$.prob
            n <- length(private$.data)

            private$.estimate <- quantile(private$.data, p, names = FALSE)

            if (private$.type == "exact") {
                private$.p_value <- get_p_binom(
                    private$.statistic, n, 1 - p, private$.side
                )
            }

            if (private$.type == "asymp") {
                z <- private$.statistic - n * (1 - p)
                correction <- if (private$.correct) {
                    switch(private$.side, lr = sign(z) * 0.5, r = 0.5, l = -0.5)
                } else 0
                z <- (z - correction) / sqrt(n * p * (1 - p))

                private$.p_value <- get_p_continous(z, "norm", private$.side)
            }
        },

        .calculate_extra = function() {
            p <- private$.prob
            n <- length(private$.data)

            d <- qnorm(1 - (1 - private$.conf_level) / 2) * sqrt(
                n * p * (1 - p)
            )
            a <- round(n * p - d)
            b <- round(n * p + 1 + d)

            sorted <- sort.int(private$.data)
            private$.conf_int <- c(
                if (a >= 1) sorted[a] else -Inf,
                if (b <= n) sorted[b] else Inf
            )
        },

        .on_null_value_change = function() {
            private$.calculate_statistic()
            private$.calculate_p()
        }
    ),
    active = list(
        #' @field prob The probability associated with the quantile.
        prob = function(value) {
            if (missing(value)) {
                private$.prob
            } else if (
                length(value) == 1 && is.finite(value) && value > 0 && value < 1
            ) {
                private$.prob <- as.numeric(value)
                if (!is.null(private$.raw_data)) {
                    private$.define()
                    private$.calculate_p()
                    private$.calculate_extra()
                }
            } else {
                stop("'prob' must be a single number between 0 and 1")
            }
        },
        #' @template active_params
        correct = function(value) {
            if (missing(value)) {
                private$.correct
            } else if (length(value) == 1 && is.logical(value)) {
                private$.correct <- as.logical(value)
                if (!is.null(private$.raw_data) && private$.type == "asymp") {
                    private$.calculate_p()
                }
            } else {
                stop("'correct' must be a single logical value")
            }
        }
    )
)