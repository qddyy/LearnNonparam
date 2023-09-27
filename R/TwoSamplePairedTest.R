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
                swapped = rep_len(FALSE, nrow(private$.data))
            )
        },

        .calculate_statistic_permu = function() {
            private$.statistic_permu <- get_arrangement(
                "permute", n_sample = private$.n_permu,
                v = c(TRUE, FALSE), m = nrow(private$.data), replace = TRUE,
                func = private$.statistic_func, func_value = numeric(1)
            )
        }
    )
)