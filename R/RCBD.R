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

            colnames(data) <- paste0("block_", seq_len(ncol(data)))
            rownames(data) <- paste0("treatment_", seq_len(nrow(data)))

            private$.data <- data
        },

        .permute = function() {
            k <- nrow(private$.data)
            b <- ncol(private$.data)

            if (is.null(private$.n_permu)) {
                col_permu <- lapply(private$.data, permutations)
                index_permu <- expand.grid(rep(list(seq_len(factorial(k))), b))

                private$.data_permu <- apply(
                    index_permu, 1, function(index) {
                        do.call(
                            data.frame, lapply(
                                seq_along(index), function(i) col_permu[[i]][i, ]
                            )
                        )
                    }
                )
            } else {
                data <- private$.data
                private$.data_permu <- lapply(
                    seq_len(private$.n_permu), function(...) {
                        do.call(data.frame, lapply(data, function(x) x[sample.int(k)]))
                    }
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
            k <- nrow(private$.data)

            private$.data <- do.call(data.frame, lapply(
                private$.data,
                function(x) score(x, n = k, method = private$.scoring)
            ))
        }
    )
)