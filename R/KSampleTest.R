#' @title KSampleTest Class
#' 
#' @description Abstract class for k-sample tests.
#' 
#' @aliases class.ksample
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

            private$.data <- `names<-`(
                unlist(
                    private$.raw_data,
                    recursive = FALSE, use.names = FALSE
                ),
                rep.int(
                    seq_along(private$.raw_data),
                    lengths(private$.raw_data, use.names = FALSE)
                )
            )
        },

        .calculate_score = function() {
            private$.data <- get_score(private$.data, private$.scoring)
        },

        .calculate_statistic = function() {
            private$.statistic <- private$.statistic_func(
                unname(private$.data), as.integer(names(private$.data))
            )
        },

        .calculate_statistic_permu = function() {
            private$.statistic <- ksample_pmt(
                unname(private$.data),
                as.integer(names(private$.data)),
                private$.statistic_func,
                private$.n_permu,
                isTRUE(getOption("LearnNonparam.pmt_progress"))
            )
        }
    )
)