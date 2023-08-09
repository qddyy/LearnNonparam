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
            dim <- dim(private$.data)
            r <- dim[1]
            c <- dim[2]

            row_sum <- .rowSums(private$.data, r, c)
            col_sum <- .colSums(private$.data, r, c)

            col_index <- rep(seq_len(c), col_sum)
            private$.data_permu <- lapply(
                permutations(
                    v = rep(seq_len(r), row_sum),
                    nsample = private$.n_permu, layout = "list"
                ),
                function(data) {
                    vapply(
                        X = split(data, col_index), USE.NAMES = FALSE,
                        FUN = tabulate, nbins = r, FUN.VALUE = integer(r)
                    )
                }
            )
        },

        .calculate_statistic = function() {
            private$.statistic <- private$.statistic_func(private$.data)
        },

        .calculate_statistic_permu = function() {
            private$.statistic_permu <- vapply(
                private$.data_permu, private$.statistic_func, numeric(1)
            )
        }
    )
)