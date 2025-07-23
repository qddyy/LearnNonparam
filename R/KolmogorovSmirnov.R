#' @title `r KolmogorovSmirnov$private_fields$.name`
#' 
#' @description Performs two-sample Kolmogorov-Smirnov test on samples.
#' 
#' @aliases distribution.ks
#' 
#' @examples
#' pmt(
#'     "distribution.ks", n_permu = 0
#' )$test(Table2.8.1)$print()
#' 
#' @export
#' 
#' @importFrom R6 R6Class


KolmogorovSmirnov <- R6Class(
    classname = "KolmogorovSmirnov",
    inherit = TwoSampleDistributionTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `KolmogorovSmirnov` object.
        #' 
        #' @template pmt_init_params
        #' 
        #' @return A `KolmogorovSmirnov` object.
        initialize = function(
            alternative = c("two_sided", "less", "greater"), n_permu = 1e4
        ) {
            self$alternative <- alternative
            self$n_permu <- n_permu
        }
    ),
    private = list(
        .name = "Two-Sample Kolmogorov-Smirnov Test",

        .define = function() {
            private$.statistic_func <- function(f, g) {
                switch(private$.alternative,
                    two_sided = function(f, g) max(abs(f - g)),
                    less = function(f, g) max(g - f),
                    greater = function(f, g) max(f - g)
                )
            }
        },

        .calculate_side = function() {
            private$.side <- "r"
        },

        .on_alternative_change = function() private$.calculate()
    )
)