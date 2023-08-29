#' @title RCBD Class
#' 
#' @description This class specializes `PermuTest` for randomized complete block design. Note that it is not recommended to create objects of this class directly. 
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom arrangements permutations


RCBD <- R6Class(
    classname = "RCBD",
    inherit = PermuTest,
    cloneable = FALSE,
    private = list(
        .name = "Randomized Complete Block Design",

        .check = function() {}, # TODO

        .feed = function(...) {
            data <- do.call(data.frame, data_to_list(...))

            dim <- dim(data)
            rownames(data) <- paste0("treatment_", seq_len(dim[1]))
            colnames(data) <- paste0("block_", seq_len(dim[2]))

            private$.raw_data <- data
        },

        .permute = function() {
            k <- nrow(private$.data)
            b <- ncol(private$.data)

            private$.data_permu <- if (is.null(private$.n_permu)) {
                apply(
                    X = expand.grid(rep(
                        list(seq_len(factorial(k))), b
                    )), MARGIN = 1,
                    FUN = function(index, cols_permu) {
                        do.call(
                            data.frame, .mapply(
                                dots = list(cols_permu, index), MoreArgs = NULL,
                                FUN = function(col_permu, i) col[i, ]
                            )
                        )
                    }, cols_permu = lapply(private$.data, permutations)
                )
            } else {
                lapply(
                    X = seq_len(private$.n_permu),
                    FUN = function(data, ...) {
                        do.call(
                            data.frame, lapply(
                                data, function(x) x[sample.int(k)]
                            )
                        )
                    }, data = private$.data
                )
            }
        },

        .calculate_statistic = function() {
            private$.statistic <- private$.statistic_func(private$.data)
        },

        .calculate_statistic_permu = function() {
            private$.statistic_permu <- vapply(
                private$.data_permu, private$.statistic_func, numeric(1)
            )
        },

        .calculate_score = function() {
            private$.data <- do.call(
                data.frame, lapply(
                    X = private$.data, FUN = score,
                    method = private$.scoring, n = nrow(private$.data)
                )
            )
        }
    )
)