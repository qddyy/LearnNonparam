#' @title `r RCBDOneWay$private_fields$.name`
#' 
#' @description Performs F statistic based one-way test on samples collected in a randomized complete block design.
#' 
#' @aliases rcbd.oneway
#' 
#' @examples
#' t <- pmt(
#'     "rcbd.oneway", n_permu = 5000
#' )$test(Table4.4.3)$print()
#' 
#' t$type <- "asymp"
#' t
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom stats pf


RCBDOneWay <- R6Class(
    classname = "RCBDOneWay",
    inherit = RCBDTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `RCBDOneWay` object.
        #' 
        #' @template pmt_init_params
        #' 
        #' @return A `RCBDOneWay` object.
        initialize = function(
            type = c("permu", "asymp"),
            n_permu = 1e4
        ) {
            self$type <- type
            self$n_permu <- n_permu
        }
    ),
    private = list(
        .name = "One-Way Test for Equal Means in RCBD",

        .define = function() {
            private$.statistic_func <- function(data) {
                k <- nrow(data)
                b <- ncol(data)

                switch(private$.type,
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
            }
        },

        .calculate_side = function() {
            private$.side <- "r"
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