#' @title ContingencyTableTest Class
#' 
#' @description Abstract class for tests on contingency tables.
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom compiler cmpfun


ContingencyTableTest <- R6Class(
    classname = "ContingencyTableTest",
    inherit = PermuTest,
    cloneable = FALSE,
    private = list(
        .preprocess = function() {
            if (length(unique(lengths(private$.raw_data))) > 1) {
                stop("All samples must be of equal length")
            }

            private$.data <- unname(
                do.call(cbind, lapply(private$.raw_data, as.integer))
            )

            if (any(private$.data < 0)) {
                private$.data <- NULL
                stop("All samples must be non-negative")
            }
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
                statistic_func = cmpfun(private$.statistic_func),
                n_permu = private$.n_permu,
                progress = isTRUE(getOption("LearnNonparam.pmt_progress"))
            )
        }
    )
)