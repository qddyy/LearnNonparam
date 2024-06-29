#' @title TwoSampleAssociationTest Class
#' 
#' @description Abstract class for two-sample association tests.
#' 
#' @aliases class.association
#' 
#' @importFrom R6 R6Class


TwoSampleAssociationTest <- R6Class(
    classname = "TwoSampleAssociationTest",
    inherit = TwoSamplePairedTest,
    cloneable = FALSE,
    private = list(
        .calculate_score = function() {
            private$.data <- data.frame(
                x =  get_score(private$.data$x, private$.scoring),
                y =  get_score(private$.data$y, private$.scoring)
            )
        },

        .calculate_statistic = function() {
            private$.statistic <- association_pmt(
                private$.data$x,
                private$.data$y,
                private$.statistic_func,
                private$.type,
                private$.n_permu,
                isTRUE(getOption("LearnNonparam.pmt_progress"))
            )
        }
    )
)