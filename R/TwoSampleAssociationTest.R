#' @title TwoSampleAssociationTest Class
#' 
#' @description Abstract class for two-sample association tests.
#' 
#' 
#' @importFrom R6 R6Class


TwoSampleAssociationTest <- R6Class(
    classname = "TwoSampleAssociationTest",
    inherit = TwoSamplePairedTest,
    cloneable = FALSE,
    private = list(
        .calculate_statistic_permu = function() {
            private$.statistic <- association_pmt(
                x = private$.data$x,
                y = private$.data$y,
                statistic_func = private$.statistic_func,
                n_permu = private$.n_permu,
                progress = isTRUE(getOption("LearnNonparam.pmt_progress"))
            )
        }
    )
)