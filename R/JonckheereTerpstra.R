#' @title `r JonckheereTerpstra$private_fields$.name`
#' 
#' @description Performs Jonckheere-Terpstra test on samples.
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
            k <- as.integer(names(private$.data)[length(private$.data)])

            I <- unlist(lapply(
                seq_len(k - 1), seq_len
            ), recursive = FALSE, use.names = FALSE)
            J <- rep.int(seq_len(k)[-1], seq_len(k - 1))

            lengths <- tabulate(as.integer(names(private$.data)))
            lengths_I <- lengths[I]
            lengths_J <- lengths[J]
            lengths_IJ <- rep.int(lengths_I, lengths_J)

            private$.statistic_func <- function(data, group) {
                split <- split.default(data, group)

                sum(`<`(
                    unlist(
                        rep.int(split[I], lengths_J),
                        recursive = FALSE, use.names = FALSE
                    ),
                    rep.int(
                        unlist(split[J], recursive = FALSE, use.names = FALSE),
                        lengths_IJ
                    )
                ))
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