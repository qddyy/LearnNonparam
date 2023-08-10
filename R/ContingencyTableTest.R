#' @title ContingencyTableTest Class
#' 
#' @description This class specializes `PermuTest` for permutation tests for contingency tables. Note that it is not recommended to create objects of this class directly. 
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom arrangements permutations


ContingencyTableTest <- R6Class(
    classname = "ContingencyTableTest",
    inherit = PermuTest,
    cloneable = FALSE,
    private = list(
        .name = "Contingency Table Permutation Test",

        .check = function() {}, # TODO

        .feed = function(table) {
            private$.data <- as.matrix(table)
        },

        .permute = function() {
            r <- nrow(private$.data)
            c <- ncol(private$.data)

            row_sum <- .rowSums(private$.data, r, c)
            col_sum <- .colSums(private$.data, r, c)

            private$.data_permu <- lapply(
                X = permutations(
                    v = rep(seq_len(r), row_sum),
                    nsample = private$.n_permu, layout = "list"
                ),
                FUN = function(data, col_index) {
                    vapply(
                        X = split(data, col_index), USE.NAMES = FALSE,
                        FUN = tabulate, nbins = r, FUN.VALUE = integer(r)
                    )
                }, col_index = rep(seq_len(c), col_sum)
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