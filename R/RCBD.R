#' @title RCBD Class
#' 
#' @description This class specializes `PermuTest` for randomized complete block design. Note that it is not recommended to create objects of this class directly. 
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class


RCBD <- R6Class(
    classname = "RCBD",
    inherit = PermuTest,
    cloneable = FALSE,
    private = list(
        .name = "Randomized Complete Block Design",

        .check = function() {},

        .preprocess = function() {
            private$.data <- unname(do_call(cbind, private$.raw_data))
        },

        .calculate_score = function() {
            private$.data <- apply(
                X = private$.data, MARGIN = 2, FUN = get_score,
                method = private$.scoring, n = nrow(private$.data)
            )
        },

        .calculate_statistic = function() {
            private$.statistic <- private$.statistic_func(private$.data)
        },

        .calculate_statistic_permu = function() {
            private$.statistic_permu <- rcbd_pmt(
                data = apply(private$.data, 2, sort),
                statistic_func = private$.statistic_func,
                n_permu = as.integer(private$.n_permu)
            )
        }
    )
)