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

        .preprocess = function() {
            super$.preprocess()

            private$.data <- do_call(data.frame, private$.data)
        },

        .calculate_score = function() {},

        .calculate_statistic = function() {
            private$.statistic <- private$.statistic_func(
                swapped = rep.int(FALSE, nrow(private$.data))
            )
        },

        .calculate_statistic_permu = function() {
            private$.statistic_permu <- paired_pmt(
                n = nrow(private$.data),
                statistic_func = private$.statistic_func,
                n_permu = private$.n_permu
            )
        }
    )
)