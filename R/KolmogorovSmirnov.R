#' @title `r KolmogorovSmirnov$private_fields$.name`
#' 
#' @description Performs two sample Kolmogorov-Smirnov test on data vectors. 
#' 
#' @aliases twosample.ks
#' 
#' @export
#' 
#' @importFrom R6 R6Class


KolmogorovSmirnov <- R6Class(
    classname = "KolmogorovSmirnov",
    inherit = TwoSampleTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `KolmogorovSmirnov` object. 
        #' 
        #' @template init_params
        #' 
        #' @return A `KolmogorovSmirnov` object. 
        initialize = function(n_permu = NULL) {
            super$initialize(alternative = "greater", n_permu = n_permu)
        }
    ),
    private = list(
        .name = "Two Sample Kolmogorov-Smirnov Test",

        .define_statistic = function() {
            m <- length(private$.data$x)
            n <- length(private$.data$y)

            tmp <- rep.int(1 / m, m + n)
            private$.statistic_func <- function(x, y) {
                max(abs(cumsum(`[<-`(tmp, order(c(x, y)) <= m, -1 / n))))
            }
        }
    )
)