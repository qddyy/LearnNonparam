#' @title `r Kuiper$private_fields$.name`
#' 
#' @description Performs two-sample Kuiper test on samples.
#' 
#' @aliases distribution.kuiper
#' 
#' @examples
#' pmt(
#'     "distribution.kuiper", n_permu = 0
#' )$test(Table2.8.1)$print()
#' 
#' @export
#' 
#' @importFrom R6 R6Class


Kuiper <- R6Class(
    classname = "Kuiper",
    inherit = TwoSampleDistributionTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `Kuiper` object.
        #' 
        #' @template pmt_init_params
        #' 
        #' @return A `Kuiper` object.
        initialize = function(
            alternative = c("two_sided", "less", "greater"), n_permu = 1e4
        ) {
            self$alternative <- alternative
            self$n_permu <- n_permu
        }
    ),
    private = list(
        .name = "Two-Sample Kuiper Test",

        .define = function() {
            private$.statistic_func <- function(...) {
                switch(private$.alternative,
                    two_sided = function(f, g) {
                        d <- f - g
                        max(d) - min(d)
                    },
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