#' @title KSampleTest Class
#' 
#' @description This class specializes `PermuTest` for k sample permutation tests. Note that it is not recommended to create objects of this class directly.
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class


KSampleTest <- R6Class(
    classname = "KSampleTest",
    inherit = PermuTest,
    cloneable = FALSE,
    private = list(
        .name = "K Sample Permutation Test",

        .check = function() {},

        .preprocess = function() {
            data <- private$.raw_data
            private$.data <- `names<-`(
                unlist(data, recursive = FALSE, use.names = FALSE),
                rep.int(seq_along(data), vapply(data, length, integer(1)))
            )
        },

        .calculate_score = function() {
            private$.data <- get_score(private$.data, method = private$.scoring)
        },

        .calculate_statistic = function() {
            private$.statistic <- private$.statistic_func(
                unname(private$.data), as.integer(names(private$.data))
            )
        },

        .calculate_statistic_permu = function() {
            private$.statistic_permu <- ksample_pmt(
                data = unname(private$.data),
                group = as.integer(names(private$.data)),
                statistic_func = private$.statistic_func,
                n_permu = private$.n_permu
            )
        }
    )
)