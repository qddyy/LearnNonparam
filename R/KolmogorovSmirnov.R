#' @title Two Sample Kolmogorov-Smirnov Test
#' 
#' @description Performs two sample Kolmogorov-Smirnov test on data vectors. 
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class


KolmogorovSmirnov <- R6Class(
    classname = "Two Sample Kolmogorov-Smirnov Test",
    inherit = TwoSampleTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `KolmogorovSmirnov` object. 
        #' 
        #' @param n_permu an integer specifying how many permutations should be used to construct the permutation distribution. If `NULL` (default) then all permutations are used.
        #' 
        #' @return A `KolmogorovSmirnov` object. 
        initialize = function(n_permu = NULL) {
            super$initialize(alternative = "greater", n_permu = n_permu)
        }
    ),
    private = list(
        .calculate_statistic = function() {
            c_xy <- c(private$.data$x, private$.data$y)
            private$.statistic_func <- function(x, y) max(abs(ecdf(x)(c_xy) - ecdf(y)(c_xy)))

            super$.calculate_statistic()
        }
    )
)