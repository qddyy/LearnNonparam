#' @title `r Correlation$private_fields$.name`
#' 
#' @description Performs correlation coefficient based two sample permutation test on data vectors. 
#' 
#' @aliases association.corr
#' 
#' @export
#' 
#' @importFrom R6 R6Class


Correlation <- R6Class(
    classname = "Correlation",
    inherit = TwoSampleAssociationTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `Correlation` object. 
        #' 
        #' @param type a character string specifying the way to calculate p-values, must be one of `"permu"` (default) or `"approx"`. 
        #' @param method a character string indicating which correlation coefficient is to be computed. Must be one of `"pearson"` (default), `"kendall"`, or `"spearman"`. 
        #' 
        #' @param alternative a character string specifying the alternative hypothesis, must be one of `"two_sided"` (default), `"greater"` or `"less"`.
        #' @param n_permu an integer specifying how many permutations should be used to construct the permutation distribution. If `NULL` (default) then all permutations are used.
        #' 
        #' @return A `Correlation` object. 
        initialize = function(
            type = c("permu", "approx"), method = c("pearson", "kendall", "spearman"),
            alternative = c("two_sided", "less", "greater"), n_permu = NULL
        ) {
            private$.type <- match.arg(type)
            private$.method <- match.arg(method)

            super$initialize(alternative = match.arg(alternative), n_permu = n_permu)
        }
    ),
    private = list(
        .name = "Two Sample Test Based on Correlation Coefficient",

        .calculate = function() {
            if (private$.method != "pearson") {
                private$.scoring <- "rank"
            }

            super$.calculate()
        },

        .calculate_score = function() {
            private$.data <- data.frame(
                x = rank(private$.data$x, ties.method = "min"),
                y = rank(private$.data$y, ties.method = "min")
            )
        },

        .calculate_statistic = function() {
            if (private$.method == "kendall") {
                x <- private$.data$x
                n <- length(x)

                order_x <- order(x)
                x_reorder <- x[order_x]

                j_index <- rep.int(seq_len(n), seq.int(0, n - 1))
                i_index <- c(lapply(seq.int(1, n - 1), seq_len), recursive = T)
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
                    approx = cor
                )
            }

            super$.calculate_statistic()
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

                less <- pnorm(z)
                greater <- pnorm(z, lower.tail = FALSE)
            } else {
                statistic <- r * sqrt((n - 2) / (1 - r^2))

                less <- pt(statistic, df = n - 2)
                greater <- pt(statistic, df = n - 2, lower.tail = FALSE)
            }
            two_sided <- 2 * min(less, greater)

            private$.p_value <- switch(private$.alternative,
                greater = greater, less = less, two_sided = two_sided
            )
        }
    )
)