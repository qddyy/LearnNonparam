#' @title Quantile Test
#' 
#' @description Performs one sample quantile test on data vectors. In addition, a confidence interval will be calculated. 
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class


Quantile <- R6Class(
    classname = "Quantile Test",
    inherit = OneSampleTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `Quantile` object. 
        #' 
        #' @param p a numeric between 0 and 1 indicating the probability. 
        #' @param null_value a number indicating the true value of the quantile. 
        #' @param conf_level a number specifying confidence level of the interval.
        #' 
        #' @param alternative a character string specifying the alternative hypothesis, must be one of `"two_sided"` (default), `"greater"` or `"less"`.
        #' 
        #' @return A `Quantile` object. 
        initialize = function(
            p = 0.5, null_value = 0,
            alternative = c("two_sided", "less", "greater"), conf_level = 0.95
        ) {
            private$.p <- p
            private$.null_value <- null_value

            super$initialize(alternative = match.arg(alternative), conf_level = conf_level)
        }
    ),
    private = list(
        .p = NULL,
        .null_value = NULL,

        .calculate_statistic = function() {
            private$.statistic <- sum(private$.data > private$.null_value)
        },

        .calculate_p = function() {
            x <- private$.statistic
            n <- length(private$.data)
            p <- private$.p

            # modified stats::binom.test
            private$.p_value <- switch(
                private$.alternative,
                less = pbinom(x, n, p),
                greater = pbinom(x - 1, n, p, lower.tail = FALSE),
                two_sided = if (p == 0) (x == 0) else if (p == 1) (x == n) else {
                    relErr <- 1 + 1e-07
                    d <- dbinom(x, n, p)
                    m <- n * p
                    if (x == m) 1 else if (x < m) {
                        i <- seq.int(from = ceiling(m), to = n)
                        y <- sum(dbinom(i, n, p) <= d * relErr)
                        pbinom(x, n, p) + pbinom(n - y, n, p, lower.tail = FALSE)
                    } else {
                        i <- seq.int(from = 0, to = floor(m))
                        y <- sum(dbinom(i, n, p) <= d * relErr)
                        pbinom(y - 1, n, p) + pbinom(x - 1, n, p, lower.tail = FALSE)
                    }
                }
            )
        },

        .calculate_ci = function() {
            n <- length(private$.data)
            beta <- 1 - (1 - private$.conf_level) / 2
            p <- private$.p
            
            d <- qnorm(beta) * sqrt(n * p * (1 - p))
            a <- round(p * n - d)
            b <- round(p * n + 1 + d)

            y <- sort(private$.data)
            private$.ci <- c(y[a], y[b])
        }
    ),
    active = list(
        #' @field p The probability. 
        p = function(value) {
            if (missing(value)) {
                private$.p
            } else {
                private$.p <- value
                private$.check()
                private$.calculate()
            }
        },
        #' @field null_value The true value of the quantile. 
        null_value = function(value) {
            if (missing(value)) {
                private$.p
            } else {
                private$.null_value <- value
                private$.check()
                private$.calculate()
            }
        }
    )
)