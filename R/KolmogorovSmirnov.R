#' @title `r KolmogorovSmirnov$private_fields$.name`
#' 
#' @description Performs two-sample Kolmogorov-Smirnov test on samples.
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
        initialize = function(
            n_permu = 0L
        ) {
            self$n_permu <- n_permu
        }
    ),
    private = list(
        .name = "Two-Sample Kolmogorov-Smirnov Test",

        .define = function() {
            m <- length(private$.data$x)
            n <- length(private$.data$y)

            geq_m <- -1 / n
            leq_m <- rep.int(1 / m, m + n)
            private$.statistic_func <- function(x, y) {
                max(abs(cumsum(`[<-`(leq_m, order(c(x, y)) > m, geq_m))))
            }
        },

        .calculate_side = function() {
            private$.side <- "r"
        }
    )
)
