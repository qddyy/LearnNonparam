#' @title TwoSampleAssociationTest Class
#' 
#' @description Abstract class for two sample tests for association.
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class


TwoSampleAssociationTest <- R6Class(
    classname = "TwoSampleAssociationTest",
    inherit = TwoSampleTest,
    cloneable = FALSE,
    private = list(
        .preprocess = function() {
            super$.preprocess()

            if (length(private$.data$x) != length(private$.data$y)) {
                stop("Both samples must be of equal length")
            }

            private$.data <- do_call(data.frame, private$.data)
        },

        .calculate_score = function() {},

        .calculate_statistic_permu = function() {
            data_y_order <- private$.data[order(private$.data$y), ]
            private$.statistic_permu <- association_pmt(
                x = data_y_order$x,
                y = data_y_order$y,
                statistic_func = private$.statistic_func,
                n_permu = private$.n_permu
            )
        }
    )
)