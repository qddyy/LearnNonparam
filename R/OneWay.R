#' @title `r OneWay$private_fields$.name`
#' 
#' @description Performs F statistic based one-way test on samples.
#' 
#' @aliases ksample.oneway
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom stats pf


OneWay <- R6Class(
    classname = "OneWay",
    inherit = KSampleTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `OneWay` object.
        #' 
        #' @template init_params
        #' 
        #' @return A `OneWay` object.
        initialize = function(
            type = c("permu", "asymp"),
            n_permu = 1e4
        ) {
            self$type <- type
            self$n_permu <- n_permu
        }
    ),
    private = list(
        .name = "One-Way Test for Equal Means",

        .define = function() {
            private$.statistic_func <- switch(private$.type,
                permu = {
                    lengths <- tabulate(as.integer(names(private$.data)))
                    function(data, group) {
                        sum(vapply(
                            X = split.default(data, group), FUN = sum,
                            FUN.VALUE = numeric(1), USE.NAMES = FALSE
                        )^2 / lengths)
                    }
                },
                asymp = function(data, group) {
                    N <- length(data)
                    split <- split(data, group)
                    k <- length(split)

                    bar_.. <- mean(data)
                    bar_i. <- unlist(lapply(
                        split, function(x) rep.int(mean(x), length(x))
                    ), recursive = FALSE, use.names = FALSE)

                    mst <- sum((bar_i. - bar_..)^2) / (k - 1)
                    mse <- sum((data - bar_i.)^2) / (N - k)
                    mst / mse
                }
            )
        },

        .calculate_side = function() {
            private$.side <- "r"
        },

        .calculate_p = function() {
            N <- length(private$.data)
            k <- as.integer(names(private$.data)[N])

            private$.p_value <- get_p_continous(
                private$.statistic, "f", "r", df1 = k - 1, df2 = N - k
            )
        }
    )
)