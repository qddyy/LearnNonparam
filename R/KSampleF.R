#' @title `r KSampleF$private_fields$.name`
#' 
#' @description Performs F statistic based k sample test on data vectors.
#' 
#' @aliases ksample.f
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom stats pf


KSampleF <- R6Class(
    classname = "KSampleF",
    inherit = KSampleTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `KSampleF` object.
        #' 
        #' @template init_params
        #' 
        #' @return A `KSampleF` object.
        initialize = function(
            type = c("permu", "asymp"),
            n_permu = 0L
        ) {
            self$type <- type
            self$n_permu <- n_permu
        }
    ),
    private = list(
        .name = "K Sample Test Based on F Statistic",

        .define = function() {
            private$.statistic_func <- switch(private$.type,
                permu = function(data, group) {
                    sum(vapply(
                        X = split(data, group),
                        FUN = function(x) sum(x)^2 / length(x),
                        FUN.VALUE = numeric(1), USE.NAMES = FALSE
                    ))
                },
                asymp = function(data, group) {
                    N <- length(data)
                    splited <- split(data, group)
                    k <- length(splited)

                    bar_.. <- mean(data)
                    bar_i. <- unlist(lapply(
                        splited, function(x) rep.int(mean(x), length(x))
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