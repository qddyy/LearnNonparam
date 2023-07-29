#' @title ANOVA for Randomized Complete Block Design
#' 
#' @description Performs F-statistic based permutation test on data for a randomized complete block design. 
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class


RCBDANOVA <- R6Class(
    classname = "ANOVA for Randomized Complete Block Design",
    inherit = RCBD,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `RCBDANOVA` object. 
        #' 
        #' @param type a character string specifying the way to calculate p-values, must be one of `"permu"` (default) or `"approx"`. 
        #' 
        #' @param n_permu an integer specifying how many permutations should be used to construct the permutation distribution. If `NULL` (default) then all permutations are used.
        #' 
        #' @return A `RCBDANOVA` object. 
        initialize = function(type = c("permu", "approx"), n_permu = NULL) {
            private$.type <- match.arg(type)

            super$initialize(alternative = "greater", n_permu = n_permu)

            private$.statistic_func <- function(df) sum(rowMeans(df)^2)
        }
    ),
    private = list(
        .calculate_statistic = function() {
            k <- nrow(private$.data)
            b <- ncol(private$.data)
            private$.statistic <- switch(private$.type,
                permu = private$.statistic_func(private$.data),
                approx = anova(lm(
                    do.call(c, private$.data) ~
                    as.factor(rep(seq_len(k), times = b)) +
                    as.factor(rep(seq_len(b), each = k))
                ))$F[1]
            )
        },

        .calculate_p = function() {
            k <- nrow(private$.data)
            b <- ncol(private$.data)

            private$.p_value <- pf(
                private$.statistic, df1 = k - 1, df2 = (k - 1) * (b - 1), , lower.tail = FALSE
            )
        }
    )
)

