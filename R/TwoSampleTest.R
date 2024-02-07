#' @title TwoSampleTest Class
#' 
#' @description Abstract class for two sample tests.
#' 
#' 
#' @export
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
                c(private$.data$x, private$.data$y), method = private$.scoring
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
            data <- unlist(private$.data, recursive = FALSE, use.names = TRUE)
            private$.statistic_permu <- twosample_pmt(
                data = unname(data),
                where_y = startsWith(names(data), "y"),
                statistic_func = private$.statistic_func,
                n_permu = private$.n_permu
            )
        }
    )
)