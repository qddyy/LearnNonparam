#' @title `r Difference$private_fields$.name`
#' 
#' @description Performs mean based two sample permutation test on data vectors. 
#' 
#' @aliases twosample.difference
#' 
#' @export
#' 
#' @importFrom R6 R6Class


Difference <- R6Class(
    classname = "Difference",
    inherit = TwoSampleTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `Difference` object. 
        #' 
        #' @template init_params
        #' 
        #' @return A `Difference` object. 
        initialize = function(
            method = c("mean", "median"),
            alternative = c("two_sided", "less", "greater"), n_permu = 0L
        ) {
            private$.method <- match.arg(method)

            super$initialize(alternative = match.arg(alternative), n_permu = n_permu)
        }
    ),
    private = list(
        .name = "Two Sample Test Based on Mean or Median",

        .define = function() {
            private$.param_name <- paste("difference", "in", paste0(private$.method, "s"))

            private$.statistic_func <- switch(private$.method,
                mean = function(x, y) mean(x) - mean(y),
                median = function(x, y) median(x) - median(y)
            )
        }
    )
)