#' @title `r AnsariBradley$private_fields$.name`
#' 
#' @description Performs Ansari-Bradley test on samples.
#' 
#' @aliases twosample.ansari
#' 
#' @export AnsariBradley
#' 
#' @importFrom R6 R6Class
#' @importFrom stats pnorm


AnsariBradley <- R6Class(
    classname = "AnsariBradley",
    inherit = TwoSampleTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `AnsariBradley` object.
        #' 
        #' @template init_params
        #' 
        #' @return A `AnsariBradley` object.
        initialize = function(
            type = c("permu", "asymp"),
            alternative = c("two_sided", "less", "greater"),
            n_permu = 0L
        ) {
            self$type <- type
            self$alternative <- alternative
            self$n_permu <- n_permu
        }
    ),
    private = list(
        .name = "Ansari-Bradley Test",
        .param_name = "ratio of scales",

        .scoring = "Ansari-Bradley rank",
        .null_value = 1,
        .link = "-",

        .calculate_score = function() {
            rank <- rank(c(private$.data$x, private$.data$y))
            ab_rank <- pmin(rank, length(rank) + 1 - rank)

            x_index <- seq_along(private$.data$x)
            private$.data <- list(x = ab_rank[x_index], y = ab_rank[-x_index])
        },

        .define = function() {
            private$.statistic_func <- function(x, y) sum(x)
        },

        .calculate_p = function() {
            m <- length(private$.data$x)
            n <- length(private$.data$y)
            N <- m + n

            even <- (N %% 2 == 0)

            mu <- if (even) m * (N + 2) / 4 else m * (N + 1)^2 / (4 * N)

            if (anyDuplicated(c(private$.data$x, private$.data$y)) == 0) {
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

            private$.p_value <- get_p_continous(z, "norm", private$.side)
        }
    )
)
