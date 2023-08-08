#' @importFrom R6 R6Class


OneSampleTest <- R6Class(
    classname = "One Sample Test",
    inherit = PermuTest,
    cloneable = FALSE,
    private = list(
        .type = "!permu",

        .check = function() {}, # TODO

        .plot = function() {},

        .feed = function(x) {
            private$.data <- x
        },

        .calculate = function() {
            private$.calculate_statistic()
            private$.calculate_p()
            private$.calculate_extra()
        }
    )
)