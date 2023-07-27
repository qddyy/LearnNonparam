#' @title Ansari-Bradley Test
#' 
#' @description Performs two sample Ansari-Bradley test on data vectors. 
#' 
#' 
#' @export AnsariBradley
#' 
#' @importFrom R6 R6Class


AnsariBradley <- R6Class(
    classname = "Ansari-Bradley Test",
    inherit = TwoSampleTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `AnsariBradley` object. 
        #' 
        #' @param type a character string specifying the way to calculate p-values, must be one of `"exact"` (default), `"approx"` or `"permu"`. Note that this parameter will be set to `"approx"` automatically when there exists ties. 
        #' @param alternative a character string specifying the alternative hypothesis, must be one of `"two_sided"` (default), `"greater"` or `"less"`.
        #' @param n_permu an integer specifying how many permutations should be used to construct the permutation distribution. If `NULL` (default) then all permutations are used.
        #' @param conf_level a number specifying confidence level of the interval.
        #' 
        #' @return A `AnsariBradley` object. 
        initialize = function(
            type = c("exact", "approx", "permu"),
            alternative = c("two_sided", "less", "greater"), n_permu = NULL, conf_level = 0.95
        ) {
            private$.type <- match.arg(type)

            super$initialize(alternative = match.arg(alternative), n_permu = n_permu, conf_level = conf_level)

            private$.scoring <- "Ansari-Bradley rank"

            private$.statistic_func <- function(x, y) sum(x)
        }
    ),
    private = list(
        .trend = "-",

        .rank = NULL,

        .calculate = function() {
            private$.rank <- rank(c(private$.data$x, private$.data$y))

            super$.calculate()
        },

        .calculate_scores = function(data) {
            m <- length(data$x)
            n <- length(data$y)
            N <- m + n

            AB_rank <- pmin(private$.rank, N + 1 - private$.rank)

            list(x = AB_rank[1:m], y = AB_rank[(m + 1):(m + n)])
        },

        .calculate_p = function() {
            m <- length(private$.data$x)
            n <- length(private$.data$y)
            N <- m + n

            # Detect ties
            ties_exist <- any(table(private$.rank) > 1)
            if (ties_exist) {
                private$.type <- "approx"
            }
    
            if (private$.type == "exact") {
                pansari <- function(q, m, n) .Call(stats:::C_pAnsari, q, m, n)

                greater <- pansari(private$.statistic, m, n)
                less <- 1 - pansari(private$.statistic - 1, m, n)
                two_sided <- min(1, 2 * (
                    if (private$.statistic > (m + 1)^2 %/% 4 + ((m * n) %/% 2) / 2) less else greater
                ))
            }

            if (private$.type == "approx") {
                even <- (N %% 2 == 0)

                mu <- if (even) m * (N + 2) / 4 else m * (N + 1)^2 / (4 * N)

                if (!ties_exist) {
                    sigma2 <- if (even) {
                        (m * n * (N + 2) * (N - 2)) / (48 * (N - 1))
                    } else {
                        (m * n * (N + 1) * (3 + N^2)) / (48 * N^2)
                    }
                } else {
                    r <- rle(sort(c(private$.data$x, private$.data$y)))
                    sigma2 <- if (even) {
                        m * n * (16 * sum(r$lengths * r$values^2) - N * (N + 2)^2) / (16 * N * (N - 1))
                    } else {
                        m * n * (16 * N * sum(r$lengths * r$values^2) - (N + 1)^4) / (16 * N^2 * (N - 1))
                    }
                }

                z <- (private$.statistic - mu) / sqrt(sigma2)

                greater <- pnorm(z)
                less <- pnorm(z, lower.tail = FALSE)
                two_sided <- 2 * min(less, greater)
            }

            private$.p_value <- switch(private$.alternative,
                greater = greater, less = less, two_sided = two_sided
            )
        }
    )
)
