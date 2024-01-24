#' @title ContingencyTableTest Class
#' 
#' @description Abstract class for permutation tests on contingency tables.
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

        .preprocess = function(table) {
            private$.data <- unname(do_call(cbind, private$.raw_data))
        },

        .calculate_statistic = function() {
            private$.statistic <- private$.statistic_func(private$.data)
        },

        .calculate_statistic_permu = function() {
            r <- nrow(private$.data)
            c <- ncol(private$.data)

            row_sum <- .rowSums(private$.data, r, c)
            col_sum <- .colSums(private$.data, r, c)

            private$.statistic_permu <- table_pmt(
                row_loc = rep.int(seq_len(r), row_sum) - 1,
                col_loc = rep.int(seq_len(c), col_sum) - 1,
                statistic_func = private$.statistic_func,
                n_permu = private$.n_permu
            )
        }
    )
)