#' @title RCBDTest Class
#' 
#' @description Abstract class for tests on samples collected in randomized complete block designs.
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom compiler cmpfun


RCBDTest <- R6Class(
    classname = "RCBDTest",
    inherit = PermuTest,
    cloneable = FALSE,
    private = list(
        .preprocess = function() {
            if (length(unique(lengths(private$.raw_data))) > 1) {
                stop("All samples must be of equal length")
            }

            private$.data <- unname(do.call(cbind, private$.raw_data))
        },

        .calculate_score = function() {
            private$.data <- apply(
                X = private$.data, MARGIN = 2, FUN = get_score,
                scoring = private$.scoring, n = nrow(private$.data)
            )
        },

        .calculate_statistic = function() {
            private$.statistic <- private$.statistic_func(private$.data)
        },

        .calculate_statistic_permu = function() {
            private$.statistic_permu <- rcbd_pmt(
                data = apply(private$.data, 2, sort),
                statistic_func = cmpfun(private$.statistic_func),
                n_permu = private$.n_permu,
                progress = isTRUE(getOption("LearnNonparam.pmt_progress"))
            )
        }
    )
)