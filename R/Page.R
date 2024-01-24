#' @title `r Page$private_fields$.name`
#' 
#' @description Performs Page test on data for a randomized complete block design.
#' 
#' @aliases rcbd.page
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom stats pnorm


Page <- R6Class(
    classname = "Page",
    inherit = RCBD,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `Page` object.
        #' 
        #' @template init_params
        #' 
        #' @return A `Page` object. 
        initialize = function(
            type = c("permu", "asymp"),
            alternative = c("two_sided", "less", "greater"),
            n_permu = 0L
        ) {
            private$.init(
                type = type, alternative = alternative, n_permu = n_permu
            )
        }
    ),
    private = list(
        .name = "Page Test",

        .scoring = "rank",

        .define = function() {
            seq_row <- seq_len(nrow(private$.data))
            private$.statistic_func <- function(data) sum(seq_row * rowSums(data))
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
