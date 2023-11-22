#' @title TwoSamplePairedTest Class
#' 
#' @description This class specializes `TwoSampleTest` for paired two sample permutation tests. Note that it is not recommended to create objects of this class directly. 
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class


TwoSamplePairedTest <- R6Class(
    classname = "TwoSamplePairedTest",
    inherit = TwoSampleTest,
    cloneable = FALSE,
    private = list(
        .name = "Paired Two Sample Permutation Test",

        .check = function() {},

        .input = function(...) {
            super$.input(...)

            private$.raw_data <- do.call(data.frame, private$.raw_data)
        },

        .calculate_score = function() {},

        .calculate_statistic = function() {
            private$.statistic <- private$.statistic_func(
                swapped = rep.int(FALSE, nrow(private$.data))
            )
        },

        .calculate_statistic_permu = function() {
            private$.statistic_permu <- paired_pmt(
                n = length(private$.data$x),
                statistic_func = private$.statistic_func,
                n_permu = as.integer(private$.n_permu)
            )
        }
    )
)