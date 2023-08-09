#' @importFrom R6 R6Class


TwoSamplePairedTest <- R6Class(
    classname = "Paired Two Sample Permutation Test",
    inherit = TwoSampleTest,
    cloneable = FALSE,
    private = list(
        .swapped_permu = NULL,

        .check = function() {}, # TODO

        .feed = function(...) {
            super$.feed(...)

            private$.data <- as.data.frame(private$.data)
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