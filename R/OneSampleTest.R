#' @importFrom R6 R6Class


OneSampleTest <- R6Class(
    classname = "One Sample Test",
    inherit = PermuTest,
    cloneable = FALSE,
    private = list(
        .type = "!permu",

        .check = function() {}, # TODO

        .feed = function(x) {
            private$.data <- x
        }
    )
)