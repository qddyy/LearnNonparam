#' @importFrom R6 R6Class
#' @importFrom arrangements permutations


RCBD <- R6Class(
    classname = "Randomized Complete Block Design",
    inherit = PermuTest,
    cloneable = FALSE,
    private = list(
        .check = function() {}, # TODO

        .feed = function(...) {
            data <- list(...)
            if (length(data) == 1 & (is.list(data[[1]]) | is.data.frame(data[[1]]))) {
                data <- data[[1]]
            } else {
                data <- do.call(data.frame, data)
            }

            dim <- dim(data)
            rownames(data) <- paste0("treatment_", seq_len(dim[1]))
            colnames(data) <- paste0("block_", seq_len(dim[2]))

            private$.data <- data
        },

        .permute = function() {
            k <- nrow(private$.data)
            b <- ncol(private$.data)

            private$.data_permu <- if (is.null(private$.n_permu)) {
                apply(
                    X = expand.grid(rep(
                        list(seq_len(factorial(k))), b
                    )), MARGIN = 1, simplify = FALSE,
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

        .calculate_scores = function() {
            private$.data <- do.call(
                data.frame, lapply(
                    X = private$.data, FUN = score,
                    n = nrow(private$.data), method = private$.scoring
                )
            )
        }
    )
)