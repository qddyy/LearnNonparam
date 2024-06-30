#' @title TwoSampleLocationTest Class
#' 
#' @description Abstract class for two-sample location tests.
#' 
#' @aliases class.twosample.location
#' 
#' @importFrom R6 R6Class


TwoSampleLocationTest <- R6Class(
    classname = "TwoSampleLocationTest",
    inherit = TwoSampleTest,
    cloneable = FALSE,
    private = list(
        .preprocess = function() {
            super$.preprocess()

            private$.data$x <- private$.data$x - private$.null_value
        }
    )
)