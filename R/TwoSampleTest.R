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

        .check = function() {}, # TODO

        .feed = function(...) {
            data <- list(...)
            if (length(data) == 1 & (is.list(data[[1]]) | is.data.frame(data[[1]]))) {
                data <- as.list(data[[1]])
            }

            names(data) <- c("x", "y")

            private$.data <- data
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