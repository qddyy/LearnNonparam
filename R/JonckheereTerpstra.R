#' @title `r JonckheereTerpstra$private_fields$.name`
#' 
#' @description Performs k sample Jonckheere-Terpstra Test on data vectors. 
#' 
#' @aliases ksample.jt
#' 
#' @export
#' 
#' @importFrom R6 R6Class


JonckheereTerpstra <- R6Class(
    classname = "JonckheereTerpstra",
    inherit = KSampleTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `JonckheereTerpstra` object. 
        #' 
        #' @param type a character string specifying the way to calculate p-values, must be one of `"permu"` (default) or `"approx"`. 
        #' 
        #' @param alternative a character string specifying the alternative hypothesis, must be one of `"two_sided"` (default), `"greater"` or `"less"`.
        #' @param n_permu an integer specifying how many permutations should be used to construct the permutation distribution. If `NULL` (default) then all permutations are used.
        #' 
        #' @return A `JonckheereTerpstra` object. 
        initialize = function(
            type = c("permu", "approx"),
            alternative = c("two_sided", "less", "greater"), n_permu = NULL
        ) {
            private$.type <- match.arg(type)

            super$initialize(alternative = match.arg(alternative), n_permu = n_permu)
        }
    ),
    private = list(
        .name = "Jonckheere-Terpstra Test",

        .calculate_statistic = function() {
            k <- as.integer(last(names(private$.data)))
            ij <- list(
                i = c(lapply(seq_len(k - 1), seq_len), recursive = TRUE),
                j = rep.int(seq_len(k)[-1], seq_len(k - 1))
            )
            private$.statistic_func <- function(data, group) {
                where <- split(seq_along(group), group)
                sum(c(.mapply(
                    FUN = function(i, j) {
                        outer(data[where[[i]]], data[where[[j]]], "<")
                    }, dots = ij, MoreArgs = NULL
                ), recursive = TRUE))
            }

            super$.calculate_statistic()
        },

        .calculate_p = function() {
            N <- length(private$.data)
            n <- tabulate(as.integer(names(private$.data)))

            z <- (private$.statistic - 1 / 4 * (N^2 - sum(n^2))) / sqrt(
                1 / 72 * (N^2 * (2 * N + 3) - sum(n^2 * (2 * n + 3)))
            )

            private$.p_value <- get_p_continous(z, "norm", private$.side)
        }
    )
)