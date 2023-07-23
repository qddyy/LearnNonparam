#' @importFrom R6 R6Class
#' @importFrom arrangements permutations


TwoSampleAssociationTest <- R6Class(
    classname = "Two Sample Permutation Test for Association",
    inherit = TwoSamplePairedTest,
    cloneable = FALSE,
    private = list(
        .x_permu = NULL,

        .check = function() {}, # TODO

        .permute = function() {
            private$.x_permu <- permutations(
                v = private$.data$x,
                nsample = private$.n_permu, layout = "list"
            )
            
            y <- private$.data$y
            private$.data_permu <- lapply(
                private$.x_permu, function(x) data.frame(x = x, y = y)
            )
        },

        .calculate_statistic_permu = function() {
            statistic_func <- private$.statistic_func
            y <- private$.data$y
            private$.statistic_permu <- sapply(
                private$.x_permu, function(x) statistic_func(x, y)
            )
        }
    )
)