#' @importFrom R6 R6Class
#' @importFrom arrangements combinations


TwoSampleTest <- R6Class(
    classname = "Two Sample Permutation Test",
    inherit = PermuTest,
    cloneable = FALSE,
    private = list(
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

            permu_index <- combinations(
                n = length(c_xy), k = length(private$.data$x),
                nsample = private$.n_permu, layout = "list"
            )

            private$.data_permu <- lapply(
                permu_index, function(index) list(
                    x = c_xy[index], y = c_xy[-index]
                )
            )
        },

        .calculate_statistic = function() {
            private$.statistic <- private$.statistic_func(private$.data$x, private$.data$y)
        },

        .calculate_statistic_permu = function() {
            statistic_func <- private$.statistic_func
            private$.statistic_permu <- vapply(
                X = private$.data_permu, FUN.VALUE = numeric(1),
                FUN = function(data) statistic_func(data$x, data$y)
            )
        },

        .calculate_score = function() {
            scores <- score(c(private$.data$x, private$.data$y), method = private$.scoring)

            x_index <- seq_along(private$.data$x)
            private$.data <- list(x = scores[x_index], y = scores[-x_index])
        }
    )
)