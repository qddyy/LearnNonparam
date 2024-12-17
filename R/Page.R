#' @title `r Page$private_fields$.name`
#' 
#' @description Performs Page test on samples collected in a randomized complete block design.
#' 
#' @aliases rcbd.page
#' 
#' @examples
#' t <- pmt(
#'     "rcbd.page", alternative = "less"
#' )$test(Table4.4.3)$print()
#' 
#' t$type <- "asymp"
#' t
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom stats pnorm


Page <- R6Class(
    classname = "Page",
    inherit = RCBDTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `Page` object.
        #' 
        #' @template pmt_init_params
        #' 
        #' @return A `Page` object. 
        initialize = function(
            type = c("permu", "asymp"),
            alternative = c("two_sided", "less", "greater"),
            n_permu = 1e4
        ) {
            self$type <- type
            self$alternative <- alternative
            self$n_permu <- n_permu
        }
    ),
    private = list(
        .name = "Page Test",

        .scoring = "rank",

        .define = function() {
            k <- nrow(private$.data)
            b <- ncol(private$.data)

            row <- seq_len(k)
            private$.statistic_func <- function(data) {
                sum(row * .rowSums(data, k, b))
            }
        },

        .calculate_p = function() {
            k <- nrow(private$.data)
            b <- ncol(private$.data)

            z <- (private$.statistic - b * k * (k + 1)^2 / 4) / sqrt(
                (k - 1) * k * (k + 1) / 12 * sum(apply(private$.data, 2, var))
            )

            private$.p_value <- get_p_continous(z, "norm", private$.side)
        }
    )
)