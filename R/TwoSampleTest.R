#' @title TwoSampleTest Class
#' 
#' @description This class specializes `PermuTest` for two sample permutation tests. Note that it is not recommended to create objects of this class directly. 
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class


TwoSampleTest <- R6Class(
    classname = "TwoSampleTest",
    inherit = PermuTest,
    cloneable = FALSE,
    private = list(
        .name = "Two Sample Permutation Test",

        .check = function() {},

        .input = function(...) {
            private$.raw_data <- setNames(get_list(...), c("x", "y"))
        },

        .calculate_score = function() {
            score <- get_score(
                c(private$.data$x, private$.data$y), method = private$.scoring
            )

            x_index <- seq_along(private$.data$x)
            private$.data <- list(x = score[x_index], y = score[-x_index])
        },

        .calculate_statistic = function() {
            private$.statistic <- private$.statistic_func(
                private$.data$x, private$.data$y
            )
        },

        .calculate_statistic_permu = function() {
            m <- length(private$.data$x)
            n <- length(private$.data$y)

            private$.statistic_permu <- get_arrangement(
                "combo", n_sample = private$.n_permu,
                v = m + n, m = m,
                func = function(index) {
                    statistic_func(c_xy[index], c_xy[-index])
                }, func_value = numeric(1),
                statistic_func = private$.statistic_func,
                c_xy = c(private$.data$x, private$.data$y)
            )
        }
    )
)