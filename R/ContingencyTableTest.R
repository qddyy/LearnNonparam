#' @title ContingencyTableTest Class
#' 
#' @description This class specializes `PermuTest` for permutation tests for contingency tables. Note that it is not recommended to create objects of this class directly. 
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class


ContingencyTableTest <- R6Class(
    classname = "ContingencyTableTest",
    inherit = PermuTest,
    cloneable = FALSE,
    private = list(
        .name = "Contingency Table Permutation Test",

        .check = function() {},

        .input = function(table) {
            private$.raw_data <- as.matrix(table)
        },

        .calculate_statistic = function() {
            private$.statistic <- private$.statistic_func(private$.data)
        },

        .calculate_statistic_permu = function() {
            r <- nrow(private$.data)
            c <- ncol(private$.data)

            row_sum <- .rowSums(private$.data, r, c)
            col_sum <- .colSums(private$.data, r, c)

            private$.statistic_permu <- get_arrangement(
                "permute", n_sample = private$.n_permu,
                v = rep.int(seq_len(r), row_sum),
                func = function(data) {
                    statistic_func(vapply(
                        X = split(data, col_index), USE.NAMES = FALSE,
                        FUN = tabulate, nbins = r, FUN.VALUE = integer(r)
                    ))
                }, func_value = numeric(1),
                statistic_func = private$.statistic_func,
                col_index = rep.int(seq_len(c), col_sum)
            )
        }
    )
)