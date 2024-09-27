#' @title TwoSamplePairedTest Class
#' 
#' @description Abstract class for paired two-sample tests.
#' 
#' @aliases class.paired
#' 
#' @importFrom R6 R6Class


TwoSamplePairedTest <- R6Class(
    classname = "TwoSamplePairedTest",
    inherit = TwoSampleTest,
    cloneable = FALSE,
    private = list(
        .preprocess = function() {
            super$.preprocess()

            if (length(private$.data$x) != length(private$.data$y)) {
                stop("Both samples must be of equal length")
            }

            private$.data <- do.call(data.frame, private$.data)
        },

        .calculate_score = function() NULL,

        .calculate_statistic = function() {
            private$.statistic <- paired_pmt(
                private$.data$x,
                private$.data$y,
                private$.statistic_func,
                if (private$.type == "permu") private$.n_permu else NA_real_,
                isTRUE(getOption("LearnNonparam.pmt_progress"))
            )
        }
    )
)