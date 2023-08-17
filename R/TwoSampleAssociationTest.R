#' @title TwoSampleAssociationTest Class
#' 
#' @description This class specializes `TwoSamplePairedTest` for two sample permutation tests for association. Note that it is not recommended to create objects of this class directly. 
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom arrangements permutations


TwoSampleAssociationTest <- R6Class(
    classname = "TwoSampleAssociationTest",
    inherit = TwoSamplePairedTest,
    cloneable = FALSE,
    private = list(
        .name = "Two Sample Permutation Test for Association",

        .check = function() {}, # TODO

        .permute = function() {
            private$.data_permu <- lapply(
                X = permutations(
                    v = private$.data$y,
                    nsample = private$.n_permu, layout = "list"
                ),
                FUN = function(x, y) {
                    data.frame(x = x, y = y)
                }, x = private$.data$x
            )
        },

        .calculate_score = function() {
            private$.data <- data.frame(
                x = score(private$.data$x, method = private$.scoring),
                y = score(private$.data$y, method = private$.scoring)
            )
        }
    )
)