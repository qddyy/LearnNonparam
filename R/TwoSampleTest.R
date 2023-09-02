#' @title TwoSampleTest Class
#' 
#' @description This class specializes `PermuTest` for two sample permutation tests. Note that it is not recommended to create objects of this class directly. 
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom arrangements combinations


TwoSampleTest <- R6Class(
    classname = "TwoSampleTest",
    inherit = PermuTest,
    cloneable = FALSE,
    private = list(
        .name = "Two Sample Permutation Test",

        .check = function() {},

        .feed = function(...) {
            private$.raw_data <- setNames(data_to_list(...), c("x", "y"))
        },

        .permute = function() {
            c_xy <- c(private$.data$x, private$.data$y)

            private$.data_permu <- lapply(
                X = combinations(
                    n = length(c_xy), k = length(private$.data$x),
                    nsample = private$.n_permu, layout = "list"
                ),
                FUN = function(index, c_xy) {
                    list(x = c_xy[index], y = c_xy[-index])
                }, c_xy = c_xy
            )
        },

        .calculate_statistic = function() {
            private$.statistic <- private$.statistic_func(
                private$.data$x, private$.data$y
            )
        },

        .calculate_statistic_permu = function() {
            private$.statistic_permu <- vapply(
                X = private$.data_permu, FUN.VALUE = numeric(1),
                FUN = function(data, f) f(data$x, data$y), f = private$.statistic_func
            )
        },

        .calculate_score = function() {
            scores <- score(c(private$.data$x, private$.data$y), method = private$.scoring)

            x_index <- seq_along(private$.data$x)
            private$.data <- list(x = scores[x_index], y = scores[-x_index])
        }
    )
)