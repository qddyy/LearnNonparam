#' @title `r JonckheereTerpstra$private_fields$.name`
#' 
#' @description Performs Jonckheere-Terpstra test on samples.
#' 
#' @aliases ksample.jt
#' 
#' @examples
#' t <- pmt(
#'     "ksample.jt", alternative = "greater"
#' )$test(Table3.4.1)$print()
#' 
#' t$type <- "asymp"
#' t
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
        #' @template pmt_init_params
        #' 
        #' @return A `JonckheereTerpstra` object.
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
        .name = "Jonckheere-Terpstra Test",

        .define = function() {
            private$.statistic_func <- function(data, group) {
                k <- group[length(group)]

                I <- unlist(lapply(seq_len(k - 1), seq_len), FALSE, FALSE)
                J <- rep.int(seq_len(k)[-1], seq_len(k - 1))

                lengths <- tabulate(group)
                lengths_IJ <- rep.int(lengths[I], lengths_J <- lengths[J])

                function(data, group) {
                    split <- split.default(data, group)
                    sum(`<`(
                        unlist(rep.int(split[I], lengths_J), FALSE, FALSE),
                        rep.int(unlist(split[J], FALSE, FALSE), lengths_IJ)
                    ))
                }
            }
        },

        .calculate_p = function() {
            N <- length(private$.data)
            n <- tabulate(attr(private$.data, "group"))

            z <- (private$.statistic - 1 / 4 * (N^2 - sum(n^2))) / sqrt(
                1 / 72 * (N^2 * (2 * N + 3) - sum(n^2 * (2 * n + 3)))
            )

            private$.p_value <- get_p_continous(z, "norm", private$.side)
        }
    )
)