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
        .preprocess = function() {
            if (length(private$.raw_data) != 1) {
                stop("Must provide only one sample")
            }

            private$.data <- private$.raw_data[[1]]
        },

        .plot = function(...) {},
        .autoplot = function(...) {}
    )
)