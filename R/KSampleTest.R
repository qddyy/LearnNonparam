#' @title KSampleTest Class
#' 
#' @description This class specializes `PermuTest` for k sample permutation tests. Note that it is not recommended to create objects of this class directly. 
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom arrangements permutations


KSampleTest <- R6Class(
    classname = "KSampleTest",
    inherit = PermuTest,
    cloneable = FALSE,
    private = list(
        .name = "K Sample Permutation Test",

        .group_permu = NULL,

        .check = function() {},

        .feed = function(...) {
            data <- data_to_list(...)

            private$.raw_data <- setNames(
                c(data, recursive = TRUE, use.names = FALSE),
                rep(seq_along(data), times = vapply(data, length, integer(1)))
            )
        },

        .permute = function() {
            private$.group_permu <- permutations(
                v = as.integer(names(private$.data)),
                nsample = private$.n_permu, layout = "list"
            )

            private$.data_permu <- lapply(
                X = private$.group_permu,
                FUN = setNames, object = unname(private$.data)
            )
        },

        .calculate_statistic = function() {
            private$.statistic <- private$.statistic_func(
                unname(private$.data), as.integer(names(private$.data))
            )
        },

        .calculate_statistic_permu = function() {
            private$.statistic_permu <- vapply(
                X = private$.group_permu,
                FUN = function(group, data, statistic_func) {
                    statistic_func(data, group)
                }, FUN.VALUE = numeric(1),
                data = unname(private$.data),
                statistic_func = private$.statistic_func
            )
        },

        .calculate_score = function() {
            private$.data <- score(private$.data, method = private$.scoring)
        }
    )
)