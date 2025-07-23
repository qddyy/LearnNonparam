#' @title TwoSampleDistributionTest Class
#' 
#' @description Abstract class for two-sample distribution tests.
#' 
#' @aliases class.distribution
#' 
#' @importFrom R6 R6Class


TwoSampleDistributionTest <- R6Class(
    classname = "TwoSampleDistributionTest",
    inherit = TwoSampleTest,
    cloneable = FALSE,
    private = list(
        .calculate_score = function() NULL,

        .calculate_statistic = function() {
            private$.statistic <- distribution_pmt(
                private$.data$x,
                private$.data$y,
                private$.statistic_func,
                if (private$.type == "permu") private$.n_permu else NA_real_,
                isTRUE(getOption("LearnNonparam.pmt_progress"))
            )
        }
    )
)