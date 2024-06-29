#' @title OneSampleTest Class
#' 
#' @description Abstract class for one-sample tests.
#' 
#' @aliases class.onesample
#' 
#' @importFrom R6 R6Class


OneSampleTest <- R6Class(
    classname = "OneSampleTest",
    inherit = PermuTest,
    cloneable = FALSE,
    public = list(
        #' @param ... ignored.
        plot = function(...) {
            stop("Can't plot a ", "<", class(self)[1], ">", " object")
        }
    ),
    private = list(
        .preprocess = function() {
            if (length(private$.raw_data) != 1) {
                stop("Must provide only one sample")
            }

            private$.data <- private$.raw_data[[1]]
        },

        .compile_statistic_closure = function() NULL
    )
)