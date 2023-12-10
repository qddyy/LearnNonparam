#' @title `r Wilcoxon$private_fields$.name`
#' 
#' @description Performs two sample Wilcoxon test, which is equivalant to the Mann-Whitney test, on data vectors. In addition, an estimation and a confidence interval of the location parameter will be calculated. 
#' 
#' @aliases twosample.wilcoxon
#' 
#' @export
#' 
#' @importFrom R6 R6Class


Wilcoxon <- R6Class(
    classname = "Wilcoxon",
    inherit = TwoSampleTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `Wilcoxon` object. 
        #' 
        #' @template init_params
        #' 
        #' @return A `Wilcoxon` object. 
        initialize = function(
            type = c("permu", "approx", "exact"), correct = TRUE,
            alternative = c("two_sided", "less", "greater"), n_permu = 0L, conf_level = 0.95
        ) {
            private$.type <- match.arg(type)
            private$.correct <- correct

            super$initialize(scoring = "rank", alternative = match.arg(alternative), n_permu = n_permu, conf_level = conf_level)
        }
    ),
    private = list(
        .name = "Two Sample Wilcoxon Test",
        .param_name = "location shift",

        .correct = NULL,

        .define = function() {
            private$.statistic_func <- function(x, y) sum(x)
        },

        .calculate_p = function() {
            m <- length(private$.data$x)
            n <- length(private$.data$y)

            statistic <- private$.statistic - m * (m + 1) / 2

            ties <- tabulate(c(private$.data$x, private$.data$y))
            if (any(ties > 1)) {
                private$.type <- "approx"
            }

            if (private$.type == "exact") {
                private$.p_value <- get_p_decrete(
                    statistic, "wilcox", private$.side, m = m, n = n
                )
            }

            if (private$.type == "approx") {
                N <- m + n

                z <- statistic - m * n / 2
                correction <- if (private$.correct) {
                    switch(private$.side, lr = sign(z) * 0.5, r = 0.5, l = -0.5)
                } else 0
                z <- (z - correction) / sqrt(
                    (m * n / 12) * ((N + 1) - sum(ties^3 - ties) / (N * (N - 1)))
                )

                private$.p_value <- get_p_continous(z, "norm", private$.side)
            }
        },

        .calculate_extra = function() {
            x <- private$.raw_data[[1]]
            y <- private$.raw_data[[2]]

            diff <- as.vector(outer(x, y, "-"))

            private$.estimate <- median(diff)

            m <- length(private$.data$x)
            n <- length(private$.data$y)

            mu <- m * n / 2
            sigma2 <- mu * (m + n + 1) / 6
            z <- qnorm(1 - (1 - private$.conf_level) / 2)

            k_a <- round(mu - z * sqrt(sigma2))
            k_b <- round(mu + z * sqrt(sigma2)) + 1

            diff_sorted <- sort(diff)

            private$.ci <- if (k_a >= 1 & k_b <= length(diff)) {
                c(diff_sorted[k_a], diff_sorted[k_b])
            } else c(NA, NA)
        }
    )
)