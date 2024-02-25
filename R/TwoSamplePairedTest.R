#' @title TwoSamplePairedTest Class
#' 
#' @description Abstract class for paired two sample tests.
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom compiler cmpfun


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

        .calculate_statistic_permu = function() {
            private$.statistic_permu <- paired_pmt(
                x = private$.data$x,
                y = private$.data$y,
                statistic_func = cmpfun(private$.statistic_func),
                n_permu = private$.n_permu
            )
        }
    )
)