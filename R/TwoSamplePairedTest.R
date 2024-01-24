#' @title TwoSamplePairedTest Class
#' 
#' @description Abstract class for paired two sample permutation tests.
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