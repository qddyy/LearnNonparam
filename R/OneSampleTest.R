#' @title OneSampleTest Class
#' 
#' @description Abstract class for one sample tests.
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class


OneSampleTest <- R6Class(
    classname = "OneSampleTest",
    inherit = PermuTest,
    cloneable = FALSE,
    private = list(
        .name = "One Sample Test",

        .preprocess = function() {
            private$.data <- private$.raw_data[[1]]
        },

        .plot = function(...) {},
        .autoplot = function(...) {}
    )
)