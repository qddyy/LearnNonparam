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

        .input = function(...) {
            super$.input(...)

            private$.raw_data <- do.call(data.frame, private$.raw_data)
        },

        .calculate_score = function() {},

        .calculate_statistic_permu = function() {
            private$.statistic_permu <- association_pmt(
                x = private$.data$x,
                y = private$.data$y,
                statistic_func = private$.statistic_func,
                n_permu = as.integer(private$.n_permu)
            )
        }
    )
)