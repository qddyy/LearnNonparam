#' @title RCBDTest Class
#' 
#' @description Abstract class for tests on samples collected in randomized complete block designs.
#' 
#' @aliases class.rcbd
#' 
#' @importFrom R6 R6Class


RCBDTest <- R6Class(
    classname = "RCBDTest",
    inherit = PermuTest,
    cloneable = FALSE,
    private = list(
        .preprocess = function() {
            if (length(unique(lengths(private$.raw_data, FALSE))) > 1) {
                stop("All samples must be of equal length")
            }

            private$.data <- unname(do.call(cbind, private$.raw_data))
        },

        .calculate_score = function() {
            private$.data <- apply(
                X = private$.data, MARGIN = 2, FUN = get_score,
                scoring = private$.scoring, n = nrow(private$.data)
            )
        },

        .calculate_statistic = function() {
            private$.statistic <- rcbd_pmt(
                private$.data,
                private$.statistic_func,
                private$.type,
                private$.n_permu,
                isTRUE(getOption("LearnNonparam.pmt_progress"))
            )
        }
    )
)