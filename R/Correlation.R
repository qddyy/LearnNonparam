#' @title `r Correlation$private_fields$.name`
#' 
#' @description Performs correlation coefficient based two sample association test on data vectors.
#' 
#' @aliases association.corr
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom stats pt pnorm


Correlation <- R6Class(
    classname = "Correlation",
    inherit = TwoSampleAssociationTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `Correlation` object.
        #' 
        #' @template init_params
        #' @param method a character string specifying the correlation coefficient to be used.
        #' 
        #' @return A `Correlation` object.
        initialize = function(
            type = c("permu", "asymp"),
            method = c("pearson", "kendall", "spearman"),
            alternative = c("two_sided", "less", "greater"),
            n_permu = 0L
        ) {
            self$type <- type
            self$method <- method
            self$alternative <- alternative
            self$n_permu <- n_permu
        }
    ),
    private = list(
        .name = "Test for Association Between Paired Samples",

        .null_value = 0,

        .preprocess = function() {
            super$.preprocess()

            if (private$.method != "pearson") {
                private$.data <- data.frame(
                    x = rank(private$.data$x),
                    y = rank(private$.data$y)
                )
            }
        },

        .define = function() {
            private$.param_name <- switch(private$.method,
                pearson = "correlation", kendall = "tau", spearman = "rho"
            )

            if (private$.method == "kendall") {
                x <- private$.data$x
                n <- length(x)

                order_x <- order(x)
                x_reorder <- x[order_x]

                i_index <- unlist(lapply(
                    seq_len(n - 1), seq_len
                ), recursive = FALSE, use.names = FALSE)
                j_index <- rep.int(seq_len(n)[-1], seq_len(n - 1))
                x_equal <- (x_reorder[i_index] == x_reorder[j_index])

                i_index <- order_x[i_index]
                j_index <- order_x[j_index]
                private$.statistic_func <- function(x, y) {
                    y_i <- y[i_index]
                    y_j <- y[j_index]

                    2 * mean(`[<-`(y_i < y_j, x_equal | y_i == y_j, 0.5)) - 1
                }
            } else {
                private$.statistic_func <- switch(private$.type,
                    permu = function(x, y) sum(x * y),
                    asymp = function(x, y) cor(x, y, method = private$.method)
                )
            }
        },

        .calculate_p = function() {
            n <- nrow(private$.data)
            r <- private$.statistic

            if (private$.method == "kendall") {
                s <- tabulate(private$.data$x)
                t <- tabulate(private$.data$y)
                a <- (sum(s * (s - 1) * (2 * s + 5)) + sum(t * (t - 1) * (2 * t + 5))) / 18
                b <- sum(s * (s - 1) * (s - 2)) * sum(t * (t - 1) * (t - 2)) / (9 * n * (n - 1) * (n - 2))
                c <- sum(s * (s - 1)) * sum(t * (t - 1)) / (2 * n * (n - 1))

                z <- r / sqrt(
                    (4 * n + 10) / (9 * n * (n - 1)) - 4 / (n^2 * (n - 1)^2) * (a - b - c)
                )
            } else {
                z <- r * sqrt(n - 1)
            }

            private$.p_value <- get_p_continous(z, "norm", private$.side)
        }
    )
)