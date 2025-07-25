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

            if (length(unique(lengths(private$.raw_data, FALSE))) > 1) {
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
            private$.statistic <- table_pmt(
                private$.data,
                private$.statistic_func,
                if (private$.type == "permu") private$.n_permu else NA_real_,
                isTRUE(getOption("LearnNonparam.pmt_progress"))
            )
        }
    )
)