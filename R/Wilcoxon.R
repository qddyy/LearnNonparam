#' @title `r Wilcoxon$private_fields$.name`
#' 
#' @description Performs two-sample wilcoxon test on samples. In addition, an estimation and a confidence interval for the location shift will be calculated.
#' 
#' @aliases twosample.wilcoxon
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom stats pnorm qnorm pwilcox dwilcox


Wilcoxon <- R6Class(
    classname = "Wilcoxon",
    inherit = TwoSampleLocationTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `Wilcoxon` object.
        #' 
        #' @template pmt_init_params
        #' @template location_init_params
        #' 
        #' @return A `Wilcoxon` object.
        initialize = function(
            type = c("permu", "asymp", "exact"),
            alternative = c("two_sided", "less", "greater"),
            null_value = 0, conf_level = 0.95,
            n_permu = 1e4, correct = TRUE
        ) {
            self$type <- type
            self$alternative <- alternative
            self$null_value <- null_value
            self$conf_level <- conf_level
            self$n_permu <- n_permu
            self$correct <- correct
        }
    ),
    private = list(
        .name = "Two-Sample Wilcoxon Test",
        .param_name = "location shift",

        .scoring = "rank",

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
                warning("There exist ties, setting 'type' to 'asymp'")
                private$.type <- "asymp"
            }

            if (private$.type == "exact") {
                private$.p_value <- get_p_decrete(
                    statistic, "wilcox", private$.side, m = m, n = n
                )
            }

            if (private$.type == "asymp") {
                N <- m + n

                z <- statistic - m * n / 2
                correction <- if (private$.correct) {
                    switch(private$.side, lr = sign(z) * 0.5, r = 0.5, l = -0.5)
                } else 0
                z <- (z - correction) / sqrt(
                    m * n / 12 * (
                        N + 1 - sum(ties^3 - ties) / (N * (N - 1))
                    )
                )

                private$.p_value <- get_p_continous(z, "norm", private$.side)
            }
        },

        .calculate_extra = function() {
            sorted_diff <- sort.int(
                outer(private$.raw_data[[1]], private$.raw_data[[2]], `-`)
            )

            private$.estimate <- median(sorted_diff)

            m <- length(private$.data$x)
            n <- length(private$.data$y)

            mu <- m * n / 2
            sigma2 <- mu * (m + n + 1) / 6
            z <- qnorm(1 - (1 - private$.conf_level) / 2)
            k_a <- round(mu - z * sqrt(sigma2))
            k_b <- round(mu + z * sqrt(sigma2)) + 1

            private$.conf_int <- c(
                if (k_a >= 1) sorted_diff[k_a] else -Inf,
                if (k_b <= m * n) sorted_diff[k_b] else Inf
            )
        }
    ),
    active = list(
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