#' @importFrom R6 R6Class
#' @importFrom arrangements permutations


ContingencyTableTest <- R6Class(
    classname = "Contingency Table Permutation Test",
    inherit = PermuTest,
    cloneable = FALSE,
    private = list(
        .check = function() {}, # TODO

        .feed = function(table) {
            private$.data <- as.matrix(table)
        },

        .permute = function() {
            row_sum <- rowSums(private$.data)
            col_sum <- colSums(private$.data)
            r <- length(row_sum)
            c <- length(col_sum)

            row_index <- rep(seq_len(r), row_sum)
            private$.data_permu <- lapply(
                permutations(
                    v = rep(seq_len(c), col_sum),
                    nsample = private$.n_permu, layout = "list"
                ),
                function(data) t(do.call(
                    data.frame, tapply(
                        data, row_index,
                        function(row) as.integer(tabulate(c(seq_len(c), row)) - 1),
                        simplify = TRUE
                    )
                ))
            )
        },

        .calculate_statistic = function() {
            private$.statistic <- private$.statistic_func(private$.data)
        },

        .calculate_statistic_permu = function() {
            private$.statistic_permu <- sapply(
                private$.data_permu, private$.statistic_func
            )
        }
    )
)