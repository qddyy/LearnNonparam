#' @title TwoSampleAssociationTest Class
#' 
#' @description This class specializes `TwoSamplePairedTest` for two sample permutation tests for association. Note that it is not recommended to create objects of this class directly.
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
        .name = "Two Sample Permutation Test for Association",

        .check = function() {},

        .preprocess = function() {
            super$.preprocess()

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