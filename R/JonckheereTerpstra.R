#' @title Jonckheere-Terpstra Test
#' 
#' @description Performs k sample Jonckheere-Terpstra Test on data vectors. 
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class


JonckheereTerpstra <- R6Class(
    classname = "Jonckheere-Terpstra Test",
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

            private$.statistic_func <- function(data, group) {
                groups <- unique(group)
                c_ij <- expand.grid(i = groups, j = groups)
                sum(apply(
                    c_ij[c_ij$i < c_ij$j, ], 1,
                    function(ij) {
                        c_xy <- expand.grid(
                            x = data[group == ij[1]],
                            y = data[group == ij[2]]
                        )
                        sum(c_xy$x < c_xy$y)
                    }
                ))
            }
        }
    ),
    private = list(
        .calculate_p = function() {
            N <- length(private$.data)
            n <- tabulate(as.integer(names(private$.data)))

            z <- (private$.statistic - 1 / 4 * (N^2 - sum(n^2))) / sqrt(
                1 / 72 * (N^2 * (2 * N + 3) - sum(n^2 * (2 * n + 3)))
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