#' @title `r ChiSquare$private_fields$.name`
#' 
#' @description Performs chi-square statistic based permutation test on contingency tables. 
#' 
#' @aliases table.chisq
#' 
#' @export
#' 
#' @importFrom R6 R6Class


ChiSquare <- R6Class(
    classname = "ChiSquare",
    inherit = ContingencyTableTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `ChiSquare` object. 
        #' 
        #' @template init_params
        #' 
        #' @return A `ChiSquare` object. 
        initialize = function(
            type = c("permu", "approx"),
            n_permu = NULL
        ) {
            super$initialize(alternative = "greater", n_permu = n_permu)
        }
    ),
    private = list(
        .name = "Contingency Table Test Based on Chi-square Statistic",

        .calculate_statistic = function() {
            dim <- dim(private$.data)
            private$.statistic_func <- function(mat) {
                row_sum <- .rowSums(mat, dim[1], dim[2])
                col_sum <- .colSums(mat, dim[1], dim[2])

                expect <- row_sum %*% matrix(col_sum, nrow = 1) / sum(mat)

                sum((mat - expect)^2 / expect)
            }

            super$.calculate_statistic()
        },

        .calculate_p = function() {
            r <- nrow(private$.data)
            c <- ncol(private$.data)

            private$.p_value <- get_p_continous(
                private$.statistic, "chisq", "r", df = (r - 1) * (c - 1)
            )
        }
    )
)