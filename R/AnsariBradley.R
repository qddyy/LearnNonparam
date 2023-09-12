#' @title `r AnsariBradley$private_fields$.name`
#' 
#' @description Performs two sample Ansari-Bradley test on data vectors. 
#' 
#' @aliases twosample.ansari
#' 
#' @export AnsariBradley
#' 
#' @importFrom R6 R6Class


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
            type = c("permu", "approx"),
            alternative = c("two_sided", "less", "greater"), n_permu = NULL, conf_level = 0.95
        ) {
            private$.type <- match.arg(type)

            super$initialize(alternative = match.arg(alternative), n_permu = n_permu, conf_level = conf_level)

            private$.scoring <- "ansari-bradley rank"
        }
    ),
    private = list(
        .name = "Ansari-Bradley Test",

        .trend = "-",

        .calculate_score = function() {
            rank <- rank(c(private$.data$x, private$.data$y))
            ab_rank <- pmin(rank, length(rank) + 1 - rank)

            x_index <- seq_along(private$.data$x)
            private$.data <- list(x = ab_rank[x_index], y = ab_rank[-x_index])
        },

        .define_statistic = function() {
            private$.statistic_func <- function(x, y) sum(x)
        },

        .calculate_p = function() {
            m <- length(private$.data$x)
            n <- length(private$.data$y)
            N <- m + n

            even <- (N %% 2 == 0)

            mu <- if (even) m * (N + 2) / 4 else m * (N + 1)^2 / (4 * N)

            if (!any(duplicated(c(private$.data$x, private$.data$y)))) {
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
