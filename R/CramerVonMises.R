#' @title `r CramerVonMises$private_fields$.name`
#' 
#' @description Performs two-sample Cramer-Von Mises test on samples.
#' 
#' @aliases distribution.cvm
#' 
#' @examples
#' pmt(
#'     "distribution.cvm", n_permu = 0
#' )$test(Table2.8.1)$print()
#' 
#' @export
#' 
#' @importFrom R6 R6Class


CramerVonMises <- R6Class(
    classname = "CramerVonMises",
    inherit = TwoSampleDistributionTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `CramerVonMises` object.
        #' 
        #' @template pmt_init_params
        #' 
        #' @return A `CramerVonMises` object.
        initialize = function(
            alternative = c("two_sided", "less", "greater"), n_permu = 1e4
        ) {
            self$alternative <- alternative
            self$n_permu <- n_permu
        }
    ),
    private = list(
        .name = "Two-Sample Cramer-Von Mises Test",

        .define = function() {
            private$.statistic_func <- function(f, g) {
                n <- c(0, `+`(
                    diff(f) * length(private$.data$x),
                    diff(g) * length(private$.data$y)
                ))
                switch(private$.alternative,
                    two_sided = function(f, g) sum((f - g)^2 * n),
                    less = function(f, g) sum((f - g)^2 * (f < g) * n),
                    greater = function(f, g) sum((f - g)^2 * (f > g) * n)
                )
            }
        },

        .calculate_side = function() {
            private$.side <- "r"
        },

        .on_alternative_change = function() private$.calculate()
    )
)