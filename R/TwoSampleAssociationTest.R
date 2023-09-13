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

        .feed = function(...) {
            super$.feed(...)

            private$.raw_data <- do.call(data.frame, private$.raw_data)
        },

        .calculate_score = function() {},

        .calculate_statistic_permu = function() {
            private$.statistic_permu <- get_arrangement(
                "permute", n_sample = private$.n_permu,
                v = private$.data$y,
                func = function(y) {
                    statistic_func(x, y)
                }, func_value = numeric(1),
                statistic_func = private$.statistic_func,
                x = private$.data$x
            )
        }
    )
)