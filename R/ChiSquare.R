#' @title Permutation Test for Contingency Tables Based on Chi-square Statistic
#' 
#' @description Performs chi-square statistic based permutation test on contingency tables. 
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class


ChiSquare <- R6Class(
    classname = "Contingency Table Permutation Test (chi-square)",
    inherit = ContingencyTableTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `ChiSquare` object. 
        #' 
        #' @param type a character string specifying the way to calculate p-values, must be one of `"permu"` (default) or `"approx"`. 
        #' 
        #' @param n_permu an integer specifying how many permutations should be used to construct the permutation distribution. If `NULL` (default) then all permutations are used.
        #' 
        #' @return A `ChiSquare` object. 
        initialize = function(
            type = c("permu", "approx"),
            n_permu = NULL
        ) {
            super$initialize(alternative = "greater", n_permu = n_permu)

            private$.statistic_func <- function(mat) {
                row_count <- apply(mat, 1, sum)
                col_count <- apply(mat, 2, sum)

                expect <- matrix(row_count, ncol = 1) %*% matrix(col_count, nrow = 1) / sum(mat)

                sum((mat - expect)^2 / expect)
            }
        }
    ),
    private = list(
        .calculate_p = function() {
            r <- nrow(private$.data)
            c <- ncol(private$.data)

            private$.p_value <- pchisq(
                private$.statistic, df = (r - 1) * (c - 1), lower.tail = FALSE
            )
        }
    )
)