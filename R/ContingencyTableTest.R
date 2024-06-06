#' @title ContingencyTableTest Class
#' 
#' @description Abstract class for tests on contingency tables.
#' 
#' @aliases class.table
#' 
#' @export
#' 
#' @importFrom R6 R6Class


ContingencyTableTest <- R6Class(
    classname = "ContingencyTableTest",
    inherit = PermuTest,
    cloneable = FALSE,
    private = list(
        .preprocess = function() {
            if (length(private$.raw_data) < 2) {
                stop("Must provide at least two samples")
            }

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

            private$.statistic <- table_pmt(
                rep.int(rep.int(seq_len(r) - 1, c), private$.data),
                rep.int(seq_len(c) - 1, .colSums(private$.data, r, c)),
                private$.statistic_func,
                private$.n_permu,
                isTRUE(getOption("LearnNonparam.pmt_progress"))
            )
        }
    )
)