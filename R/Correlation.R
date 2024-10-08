#' @title `r Correlation$private_fields$.name`
#' 
#' @description Performs correlation coefficient based two-sample association test on samples.
#' 
#' @aliases association.corr
#' 
#' @examples
#' pmt(
#'     "association.corr", method = "pearson",
#'     alternative = "greater", n_permu = 10000
#' )$test(Table5.1.2)$print()
#' 
#' t <- pmt(
#'     "association.corr", method = "spearman",
#'     alternative = "two_sided", n_permu = 10000
#' )$test(Table5.1.2)$print()
#' 
#' t$type <- "asymp"
#' t
#' 
#' t <- pmt(
#'     "association.corr", method = "kendall",
#'     alternative = "greater", n_permu = 0
#' )$test(Table5.2.2)$print()
#' 
#' t$type <- "asymp"
#' t
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom stats pnorm


Correlation <- R6Class(
    classname = "Correlation",
    inherit = TwoSampleAssociationTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `Correlation` object.
        #' 
        #' @template pmt_init_params
        #' @param method a character string specifying the correlation coefficient to be used.
        #' 
        #' @return A `Correlation` object.
        initialize = function(
            type = c("permu", "asymp"),
            method = c("pearson", "kendall", "spearman"),
            alternative = c("two_sided", "less", "greater"),
            n_permu = 1e4
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

        .define = function() {
            private$.param_name <- switch(private$.method,
                pearson = "correlation", kendall = "tau", spearman = "rho"
            )

            if (private$.method != "pearson") {
                private$.scoring <- "rank"
                private$.calculate_score()
            } else {
                private$.scoring <- "none"
            }

            if (private$.method != "kendall") {
                private$.statistic_func <- switch(private$.type,
                    permu = function(x, y) sum(x * y),
                    asymp = function(x, y) cor(x, y, method = private$.method)
                )
            } else {
                n <- nrow(private$.data)

                sorted <- sort.int(private$.data$x, index.return = TRUE)

                i_index <- unlist(lapply(seq_len(n - 1), seq_len), FALSE, FALSE)
                j_index <- rep.int(seq_len(n)[-1], seq_len(n - 1))

                x_equal <- (sorted$x[i_index] == sorted$x[j_index])

                i_index <- sorted$ix[i_index]
                j_index <- sorted$ix[j_index]

                frac_2_length <- 4 / (n * (n - 1))
                private$.statistic_func <- function(x, y) {
                    y_i <- y[i_index]
                    y_j <- y[j_index]

                    frac_2_length * sum(
                        `[<-`(y_i < y_j, x_equal | y_i == y_j, 0.5)
                    ) - 1
                }
            }
        },

        .calculate_p = function() {
            n <- nrow(private$.data)
            r <- private$.statistic

            if (private$.method == "kendall") {
                s <- tabulate(private$.data$x)
                t <- tabulate(private$.data$y)
                a <- `+`(
                    sum(s * (s - 1) * (2 * s + 5)),
                    sum(t * (t - 1) * (2 * t + 5))
                ) / 18
                b <- `*`(
                    sum(s * (s - 1) * (s - 2)),
                    sum(t * (t - 1) * (t - 2))
                ) / (9 * n * (n - 1) * (n - 2))
                c <- `*`(
                    sum(s * (s - 1)),
                    sum(t * (t - 1))
                ) / (2 * n * (n - 1))

                z <- r / sqrt(
                    (4 * n + 10) / (9 * n * (n - 1)) -
                        4 / (n^2 * (n - 1)^2) * (a - b - c)
                )
            } else {
                z <- r * sqrt(n - 1)
            }

            private$.p_value <- get_p_continous(z, "norm", private$.side)
        }
    )
)