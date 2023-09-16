#' @title `r RCBDANOVA$private_fields$.name`
#' 
#' @description Performs F-statistic based permutation test on data for a randomized complete block design. 
#' 
#' @aliases rcbd.anova
#' 
#' @export
#' 
#' @importFrom R6 R6Class


RCBDANOVA <- R6Class(
    classname = "RCBDANOVA",
    inherit = RCBD,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `RCBDANOVA` object. 
        #' 
        #' @template init_params
        #' 
        #' @return A `RCBDANOVA` object. 
        initialize = function(type = c("permu", "approx"), n_permu = NULL) {
            private$.type <- match.arg(type)

            super$initialize(alternative = "greater", n_permu = n_permu)
        }
    ),
    private = list(
        .name = "ANOVA for Randomized Complete Block Design",

        .define_statistic = function() {
            private$.statistic_func <- switch(private$.type,
                permu = function(data) sum(rowMeans(data)^2),
                approx = function(df) {
                    b <- ncol(df)

                    bar_i. <- rowMeans(df)
                    bar_.j <- colMeans(df)
                    bar_.. <- mean(bar_i.)

                    sst <- b * sum((bar_i. - bar_..)^2)
                    sse <- sum((df - outer(bar_i., bar_.j, "+") + bar_..)^2)
                    (b - 1) * sst / sse
                }
            )
        },

        .calculate_p = function() {
            k <- nrow(private$.data)
            b <- ncol(private$.data)

            private$.p_value <- get_p_continous(
                private$.statistic, "f", "r", df1 = k - 1, df2 = (k - 1) * (b - 1)
            )
        }
    )
)

