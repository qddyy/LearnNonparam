#' @title `r Friedman$private_fields$.name`
#' 
#' @description Performs Friedman test on data for a randomized complete block design. 
#' 
#' @aliases rcbd.friedman
#' 
#' @export
#' 
#' @importFrom R6 R6Class


Friedman <- R6Class(
    classname = "Friedman",
    inherit = RCBD,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `Friedman` object. 
        #' 
        #' @template init_params
        #' 
        #' @return A `Friedman` object. 
        initialize = function(
            type = c("permu", "approx"),
            n_permu = NULL
        ) {
            private$.type <- match.arg(type)

            super$initialize(scoring = "rank", alternative = "greater", n_permu = n_permu)
        }
    ),
    private = list(
        .name = "Friedman Test",

        .define = function() {
            private$.statistic_func <- switch(private$.type,
                permu = function(data) sum(rowMeans(data)^2),
                approx = function(df) {
                    ncol(df)^2 / sum(vapply(df, var, numeric(1))) *
                    sum((rowMeans(df) - (nrow(df) + 1) / 2)^2)
                }
            )
        },

        .calculate_p = function() {
            k <- nrow(private$.data)

            private$.p_value <- get_p_continous(
                private$.statistic, "chisq", "r",  df = k - 1
            )
        }
    )
)
