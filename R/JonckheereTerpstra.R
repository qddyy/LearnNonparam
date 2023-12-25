#' @title `r JonckheereTerpstra$private_fields$.name`
#' 
#' @description Performs k sample Jonckheere-Terpstra Test on data vectors. 
#' 
#' @aliases ksample.jt
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom stats pnorm


JonckheereTerpstra <- R6Class(
    classname = "JonckheereTerpstra",
    inherit = KSampleTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `JonckheereTerpstra` object. 
        #' 
        #' @template init_params
        #' 
        #' @return A `JonckheereTerpstra` object. 
        initialize = function(
            type = c("permu", "asymp"),
            alternative = c("two_sided", "less", "greater"), n_permu = 0L
        ) {
            private$.type <- match.arg(type)

            super$initialize(alternative = match.arg(alternative), n_permu = n_permu)
        }
    ),
    private = list(
        .name = "Jonckheere-Terpstra Test",

        .define = function() {
            k <- as.integer(get_last(names(private$.data)))
            ij <- list(
                i = unlist(lapply(
                    seq_len(k - 1), seq_len
                ), recursive = FALSE, use.names = FALSE),
                j = rep.int(seq_len(k)[-1], seq_len(k - 1))
            )
            private$.statistic_func <- function(data, group) {
                where <- split(seq_along(group), group)
                sum(unlist(.mapply(
                    FUN = function(i, j) {
                        outer(data[where[[i]]], data[where[[j]]], "<")
                    }, dots = ij, MoreArgs = NULL
                ), recursive = FALSE, use.names = FALSE))
            }
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