#' @title Wilcoxon Test
#' 
#' @description Performs two sample Wilcoxon test, which is equivalant to the Mann-Whitney test, on data vectors. In addition, an estimation and a confidence interval of the location parameter will be calculated. 
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class


Wilcoxon <- R6Class(
    classname = "Two Sample Wilcoxon Test",
    inherit = TwoSampleTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `Wilcoxon` object. 
        #' 
        #' @param type a character string specifying the way to calculate p-values, must be one of `"exact"` (default), `"approx"` or `"permu"`. Note that this parameter will be set to `"approx"` automatically when there exists ties. 
        #' @param correct a logical indicating whether to apply continuity correction in the normal approximation for the p-value.
        #' 
        #' @param alternative a character string specifying the alternative hypothesis, must be one of `"two_sided"` (default), `"greater"` or `"less"`.
        #' @param n_permu an integer specifying how many permutations should be used to construct the permutation distribution. If `NULL` (default) then all permutations are used.
        #' @param conf_level a number specifying confidence level of the interval.
        #' 
        #' @return A `Wilcoxon` object. 
        initialize = function(
            type = c("exact", "approx", "permu"), correct = TRUE,
            alternative = c("two_sided", "less", "greater"), n_permu = NULL, conf_level = 0.95
        ) {
            private$.type <- match.arg(type)
            private$.correct <- correct

            super$initialize(scoring = "rank", alternative = match.arg(alternative), n_permu = n_permu, conf_level = conf_level)

            private$.statistic_func <- function(x, y) {
                m <- length(x)
                sum(x) - m * (m + 1) / 2
            }
        }
    ),
    private = list(
        .correct = NULL,

        .calculate_p = function() {
            m <- length(private$.data$x)
            n <- length(private$.data$y)
            N <- m + n

            # Detect ties
            ties <- table(c(private$.data$x, private$.data$y))
            if (any(ties > 1)) {
                private$.type <- "approx"
            }
    
            if (private$.type == "exact") {
                less <- pwilcox(private$.statistic, m, n)
                greater <- pwilcox(private$.statistic - 1, m, n, lower.tail = FALSE)
                two_sided <- min(1, 2 * (
                    if (private$.statistic > m * n / 2) greater else less
                ))
            }

            if (private$.type == "approx") {
                z <- private$.statistic - m * n / 2
                correction <- if (private$.correct) switch(
                    private$.alternative,
                    two_sided = sign(z) * 0.5, greater = 0.5, less = -0.5
                ) else 0
                z <- (z - correction) / sqrt(
                    (m * n / 12) * ((N + 1) - sum(ties^3 - ties) / (N * (N - 1)))
                )

                less <- pnorm(z)
                greater <- pnorm(z, lower.tail = FALSE)
                two_sided <- 2 * min(less, greater)
            }

            private$.p_value <- switch(private$.alternative,
                greater = greater, less = less, two_sided = two_sided
            )
        },

        .calculate_extra = function() {
            combinations_xy <- expand.grid(x = private$.data$x, y = private$.data$y)
            diff <- combinations_xy$x - combinations_xy$y

            private$.estimate <- median(diff)


            m <- length(private$.data$x)
            n <- length(private$.data$y)

            mu <- m * n / 2
            sigma2 <- mu * (m + n + 1) / 6
            z <- qnorm(1 - (1 - private$.conf_level) / 2)

            k_a <- round(mu - z * sqrt(sigma2))
            k_b <- round(mu + z * sqrt(sigma2)) + 1

            diff_sorted <- sort(diff)

            private$.ci <- c(diff_sorted[k_a], diff_sorted[k_b])
        }
    )
)