#' @title `r RCBDF$private_fields$.name`
#' 
#' @description Performs F statistic based test on samples collected in a randomized complete block design.
#' 
#' @aliases rcbd.f
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom stats pf


RCBDF <- R6Class(
    classname = "RCBDF",
    inherit = RCBDTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `RCBDF` object.
        #' 
        #' @template init_params
        #' 
        #' @return A `RCBDF` object.
        initialize = function(
            type = c("permu", "asymp"),
            n_permu = 0L
        ) {
            self$type <- type
            self$n_permu <- n_permu
        }
    ),
    private = list(
        .name = "Test for RCBD Based on F Statistic",

        .define = function() {
            k <- nrow(private$.data)
            b <- ncol(private$.data)
            private$.statistic_func <- switch(private$.type,
                permu = function(data) sum(.rowMeans(data, k, b)^2),
                asymp = function(data) {
                    bar_i. <- .rowMeans(data, k, b)
                    bar_.j <- .colMeans(data, k, b)
                    bar_.. <- mean(bar_i.)

                    sst <- b * sum((bar_i. - bar_..)^2)
                    sse <- sum((data - outer(bar_i., bar_.j, `+`) + bar_..)^2)
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

