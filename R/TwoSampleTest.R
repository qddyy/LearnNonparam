#' @title TwoSampleTest Class
#' 
#' @description Abstract class for two-sample tests.
#' 
#' 
#' @importFrom R6 R6Class


TwoSampleTest <- R6Class(
    classname = "TwoSampleTest",
    inherit = PermuTest,
    cloneable = FALSE,
    private = list(
        .preprocess = function() {
            if (length(private$.raw_data) != 2) {
                stop("Must provide two samples")
            }

            private$.data <- `names<-`(private$.raw_data, c("x", "y"))
        },

        .calculate_score = function() {
            score <- get_score(
                c(private$.data$x, private$.data$y), private$.scoring
            )

            x_index <- seq_along(private$.data$x)
            private$.data <- list(x = score[x_index], y = score[-x_index])
        },

        .calculate_statistic = function() {
            private$.statistic <- private$.statistic_func(
                private$.data$x, private$.data$y
            )
        },

        .calculate_statistic_permu = function() {
            private$.statistic <- twosample_pmt(
                x = private$.data$x,
                y = private$.data$y,
                statistic_func = private$.statistic_func,
                n_permu = as.integer(private$.n_permu),
                progress = isTRUE(getOption("LearnNonparam.pmt_progress"))
            )
        }
    )
)