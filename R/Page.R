#' @title `r Page$private_fields$.name`
#' 
#' @description Performs Page test on data for a randomized complete block design. 
#' 
#' @aliases rcbd.page
#' 
#' @export
#' 
#' @importFrom R6 R6Class


Page <- R6Class(
    classname = "Page",
    inherit = RCBD,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `Page` object. 
        #' 
        #' @param type a character string specifying the way to calculate p-values, must be one of `"permu"` (default) or `"approx"`. 
        #' 
        #' @param alternative a character string specifying the alternative hypothesis, must be one of `"two_sided"` (default), `"greater"` or `"less"`.
        #' @param n_permu an integer specifying how many permutations should be used to construct the permutation distribution. If `NULL` (default) then all permutations are used.
        #' 
        #' @return A `Page` object. 
        initialize = function(
            type = c("permu", "approx"),
            alternative = c("two_sided", "less", "greater"), n_permu = NULL
        ) {
            private$.type <- match.arg(type)

            super$initialize(scoring = "rank", alternative = match.arg(alternative), n_permu = n_permu)
        }
    ),
    private = list(
        .name = "Page Test",

        .calculate_statistic = function() {
            seq_row <- seq_len(nrow(private$.data))
            private$.statistic_func <- function(df) sum(seq_row * rowSums(df))

            super$.calculate_statistic()
        },

        .calculate_p = function() {
            k <- nrow(private$.data)
            b <- ncol(private$.data)
            
            z <- (private$.statistic - b * k * (k + 1)^2 / 4) / sqrt(
                (k - 1) * k * (k + 1) / 12 * sum(vapply(private$.data, var, numeric(1)))
            )

            less <- pnorm(z)
            greater <- pnorm(z, lower.tail = FALSE)
            two_sided <- 2 * min(less, greater)

            private$.p_value <- switch(private$.alternative,
                greater = greater, less = less, two_sided = two_sided
            )
        }
    )
)
