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
            if (is.null(private$.n_permu)) {
                private$.swapped_permu <- as.matrix(
                    expand.grid(rep(list(c(TRUE, FALSE)), nrow(private$.data)))
                )
            } else {
                private$.swapped_permu <- matrix(
                    as.logical(rbinom(private$.n_permu * nrow(private$.data), 1, 0.5)),
                    nrow = private$.n_permu, byrow = TRUE
                )
            }

            x <- private$.data$x
            y <- private$.data$y
            private$.data_permu <- apply(
                private$.swapped_permu, 1, function(is_swapped) data.frame(
                    x = ifelse(is_swapped, y, x), y = ifelse(is_swapped, x, y)
                ), simplify = FALSE
            )
        }
    )
)