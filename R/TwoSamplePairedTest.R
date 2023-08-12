#' @title TwoSamplePairedTest Class
#' 
#' @description This class specializes `TwoSampleTest` for paired two sample permutation tests. Note that it is not recommended to create objects of this class directly. 
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class


TwoSamplePairedTest <- R6Class(
    classname = "TwoSamplePairedTest",
    inherit = TwoSampleTest,
    cloneable = FALSE,
    private = list(
        .name = "Paired Two Sample Permutation Test",

        .swapped_permu = NULL,

        .check = function() {}, # TODO

        .feed = function(...) {
            super$.feed(...)

            private$.raw_data <- do.call(data.frame, private$.raw_data)
        },

        .permute = function() {
            private$.swapped_permu <- if (is.null(private$.n_permu)) {
                expand.grid(rep(list(c(TRUE, FALSE)), nrow(private$.data)))
            } else {
                matrix(as.logical(rbinom(private$.n_permu * nrow(private$.data), 1, 0.5)), nrow = private$.n_permu)
            }

            private$.data_permu <- apply(
                X = private$.swapped_permu, MARGIN = 1,
                FUN = function(is_swapped, x, y) {
                    data.frame(
                        x = `[<-`(x, is_swapped, y[is_swapped]),
                        y = `[<-`(y, is_swapped, x[is_swapped])
                    )
                }, x = private$.data$x, y = private$.data$y
            )
        }
    )
)