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
        },

        #' @description Draw the empirical cumulative distribution function of each of the data fed. 
        #' 
        #' @return The object itself (invisibly).
        plot_ecdfs = function() {
            x <- private$.data$x
            y <- private$.data$y

            max <- max(max(x), max(y))
            min <- min(min(x), min(y))
            range <- max - min

            ecdfs <- ggplot() +
                geom_function(fun = ecdf(x), mapping = aes(color = "x")) +
                geom_function(fun = ecdf(y), mapping = aes(color = "y")) +
                xlim(c(min - range * 0.1, max + range * 0.1)) +
                labs(x = "", y = "")
            print(ecdfs)

            invisible(self)
        }
    ),
    private = list(
        .calculate = function() {
            c_xy <- c(private$.data$x, private$.data$y)
            private$.statistic_func <- function(x, y) max(abs(ecdf(x)(c_xy) - ecdf(y)(c_xy)))

            super$.calculate()
        }
    )
)