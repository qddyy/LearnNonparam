#' @title `r AndersonDarling$private_fields$.name`
#' 
#' @description Performs two-sample Anderson-Darling test on samples.
#' 
#' @aliases distribution.ad
#' 
#' @examples
#' pmt(
#'     "distribution.ad", n_permu = 0
#' )$test(Table2.8.1)$print()
#' 
#' @export
#' 
#' @importFrom R6 R6Class


AndersonDarling <- R6Class(
    classname = "AndersonDarling",
    inherit = TwoSampleDistributionTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `AndersonDarling` object.
        #' 
        #' @template pmt_init_params
        #' 
        #' @return A `AndersonDarling` object.
        initialize = function(
            alternative = c("two_sided", "less", "greater"), n_permu = 1e4
        ) {
            self$alternative <- alternative
            self$n_permu <- n_permu
        }
    ),
    private = list(
        .name = "Two-Sample Anderson-Darling Test",

        .define = function() {
            private$.statistic_func <- function(f, g) {
                m <- length(private$.data$x)
                n <- length(private$.data$y)

                h <- (f * m + g * n) / (m + n)
                w <- m * n / (m + n)^2 / (h * (1 - h))
                w[!is.finite(w)] <- 0
                w <- w * c(0, diff(f) * m + diff(g) * n)

                switch(private$.alternative,
                    two_sided = function(f, g) sum((f - g)^2 * w),
                    less = function(f, g) sum((f - g)^2 * (f < g) * w),
                    greater = function(f, g) sum((f - g)^2 * (f > g) * w)
                )
            }
        },

        .calculate_side = function() {
            private$.side <- "r"
        },

        .on_alternative_change = function() private$.calculate()
    )
)