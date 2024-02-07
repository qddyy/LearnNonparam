#' @title KSampleTest Class
#' 
#' @description Abstract class for k sample permutation tests.
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class


KSampleTest <- R6Class(
    classname = "KSampleTest",
    inherit = PermuTest,
    cloneable = FALSE,
    private = list(
        .preprocess = function() {
            if (length(private$.raw_data) < 3) {
                stop("Must provide at least three samples")
            }

            private$.data <- unlist(
                private$.raw_data, recursive = FALSE, use.names = FALSE
            )
            names(private$.data) <- rep.int(
                seq_along(private$.raw_data),
                lengths(private$.raw_data, use.names = FALSE)
            )
        },

        .calculate_score = function() {
            private$.data <- get_score(private$.data, method = private$.scoring)
        },

        .calculate_statistic = function() {
            private$.statistic <- private$.statistic_func(
                unname(private$.data), as.integer(names(private$.data))
            )
        },

        .calculate_statistic_permu = function() {
            private$.statistic_permu <- ksample_pmt(
                data = unname(private$.data),
                group = as.integer(names(private$.data)),
                statistic_func = private$.statistic_func,
                n_permu = private$.n_permu
            )
        }
    )
)