#' @importFrom R6 R6Class
#' @importFrom arrangements permutations


KSampleTest <- R6Class(
    classname = "K Sample Permutation Test",
    inherit = PermuTest,
    cloneable = FALSE,
    private = list(
        .group_permu = NULL,

        .check = function() {}, # TODO

        .feed = function(...) {
            data <- list(...)
            if (length(data) == 1 & (is.list(data[[1]]) | is.data.frame(data[[1]]))) {
                data <- as.list(data[[1]])
            }

            private$.data <- setNames(
                c(data, recursive = TRUE, use.names = FALSE),
                rep(seq_along(data), times = vapply(data, length, integer(1)))
            )
        },

        .permute = function() {
            private$.group_permu <- permutations(
                v = as.integer(names(private$.data)),
                nsample = private$.n_permu, layout = "list"
            )

            data <- unname(private$.data)
            private$.data_permu <- lapply(
                private$.group_permu,
                function(group) setNames(data, group)
            )
        },

        .calculate_statistic = function() {
            private$.statistic <- private$.statistic_func(
                unname(private$.data), as.integer(names(private$.data))
            )
        },

        .calculate_statistic_permu = function() {
            data <- unname(private$.data)
            statistic_func <- private$.statistic_func
            private$.statistic_permu <- vapply(
                X = private$.group_permu, FUN.VALUE = numeric(1),
                FUN = function(group) statistic_func(data, group)
            )
        },

        .calculate_score = function() {
            private$.data <- score(private$.data, method = private$.scoring)
        }
    )
)