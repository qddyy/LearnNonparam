#' @title TwoSampleAssociationTest Class
#' 
#' @description Abstract class for two-sample association tests.
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom compiler cmpfun


TwoSampleAssociationTest <- R6Class(
    classname = "TwoSampleAssociationTest",
    inherit = TwoSamplePairedTest,
    cloneable = FALSE,
    private = list(
        .calculate_statistic_permu = function() {
            data_y_order <- private$.data[order(private$.data$y), ]
            private$.statistic_permu <- association_pmt(
                x = data_y_order$x,
                y = data_y_order$y,
                statistic_func = cmpfun(private$.statistic_func),
                n_permu = private$.n_permu
            )
        }
    )
)