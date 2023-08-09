#' @importFrom R6 R6Class
#' @importFrom arrangements permutations


TwoSampleAssociationTest <- R6Class(
    classname = "Two Sample Permutation Test for Association",
    inherit = TwoSamplePairedTest,
    cloneable = FALSE,
    private = list(
        .check = function() {}, # TODO

        .permute = function() {
            private$.data_permu <- lapply(
                X = permutations(
                    v = private$.data$x,
                    nsample = private$.n_permu, layout = "list"
                ),
                FUN = function(x, y) {
                    data.frame(x = x, y = y)
                }, y = private$.data$y
            )
        }
    )
)