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
            data <- private$.data

            if (is.null(private$.n_permu)) {
                number_permu <- factorial(nrow(data))
                permu_index <- expand.grid(rep(list(seq_len(number_permu)), ncol(private$.data)))

                private$.data_permu <- apply(
                    permu_index, 1, function(index) do.call(
                        data.frame, lapply(
                            seq_along(index), function(i) permutations(
                                v = data[, i], index = index[i]
                            )
                        )
                    ), simplify = FALSE
                )
            } else {
                private$.data_permu <- replicate(
                    private$.n_permu, do.call(
                        data.frame, lapply(
                            data, function(x) permutations(v = x, nsample = 1)
                        )
                    ), simplify = FALSE
                )
            }
        },

        .calculate_statistic = function() {
            private$.statistic <- private$.statistic_func(private$.data)
        },

        .calculate_statistic_permu = function() {
            private$.statistic_permu <- sapply(
                private$.data_permu, private$.statistic_func
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