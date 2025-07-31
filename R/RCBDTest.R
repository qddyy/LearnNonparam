#' @title RCBDTest Class
#' 
#' @description Abstract class for tests on samples collected in randomized complete block designs.
#' 
#' @aliases class.rcbd
#' 
#' @importFrom R6 R6Class


RCBDTest <- R6Class(
    classname = "RCBDTest",
    inherit = PermuTest,
    cloneable = FALSE,
    private = list(
        .preprocess = function() {
            if (length(private$.raw_data) < 2) {
                stop("Must provide at least two samples")
            }

            if (length(unique(lengths(private$.raw_data, FALSE))) > 1) {
                stop("All samples must be of equal length")
            }

            private$.data <- unname(do.call(cbind, private$.raw_data))
        },

        .calculate_score = function() {
            private$.data <- `dim<-`(apply(
                X = private$.data, MARGIN = 2,
                FUN = get_score, scoring = private$.scoring
            ), c(nrow(private$.data), ncol(private$.data)))
        },

        .calculate_statistic = function() {
            private$.statistic <- rcbd_pmt(
                private$.data,
                private$.statistic_func,
                if (private$.type == "permu") private$.n_permu else NA_real_,
                isTRUE(getOption("LearnNonparam.pmt_progress"))
            )
        }
    )
)