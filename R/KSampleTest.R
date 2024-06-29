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

            private$.data <- unlist(private$.raw_data, FALSE, FALSE)
            attr(private$.data, "group") <- rep.int(
                seq_along(private$.raw_data), lengths(private$.raw_data, FALSE)
            )
        },

        .calculate_score = function() {
            private$.data <- `attr<-`(
                get_score(private$.data, private$.scoring),
                "group", attr(private$.data, "group")
            )
        },

        .calculate_statistic = function() {
            private$.statistic <- ksample_pmt(
                private$.data,
                attr(private$.data, "group"),
                private$.statistic_func,
                private$.type,
                private$.n_permu,
                isTRUE(getOption("LearnNonparam.pmt_progress"))
            )
        }
    )
)