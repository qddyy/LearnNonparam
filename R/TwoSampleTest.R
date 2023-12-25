#' @title TwoSampleTest Class
#' 
#' @description This class specializes `PermuTest` for two sample permutation tests. Note that it is not recommended to create objects of this class directly. 
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
        .name = "Two Sample Permutation Test",

        .check = function() {},

        .preprocess = function() {
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
            n_1 <- length(private$.data$x)
            n_2 <- length(private$.data$y)
            private$.statistic_permu <- twosample_pmt(
                data = c(private$.data$x, private$.data$y),
                where_y = rep.int(c(FALSE, TRUE), c(n_1, n_2)),
                statistic_func = private$.statistic_func,
                n_permu = private$.n_permu
            )
        }
    )
)