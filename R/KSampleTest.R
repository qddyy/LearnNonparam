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

        .feed = function(...) {
            data <- get_data_from(...)

            private$.raw_data <- setNames(
                c(data, recursive = TRUE, use.names = FALSE),
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
            private$.statistic_permu <- get_arrangement(
                "permute", n_sample = private$.n_permu,
                v = as.integer(names(private$.data)),
                func = function(group) {
                    statistic_func(data, group)
                }, func_value = numeric(1),
                statistic_func = private$.statistic_func,
                data = unname(private$.data)
            )
        }
    )
)