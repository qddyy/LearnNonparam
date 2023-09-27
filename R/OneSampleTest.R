#' @title OneSampleTest Class
#' 
#' @description This class specializes `PermuTest` for one sample tests. Note that it is not recommended to create objects of this class directly. 
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
        .type = "!permu",

        .check = function() {},

        .plot = function() {},

        .input = function(x) {
            private$.raw_data <- x
        }
    )
)