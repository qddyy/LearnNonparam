#' @title `r PairedComparison$private_fields$.name`
#' 
#' @description Performs two sample paired comparison on data vectors. 
#' 
#' @aliases paired.comparison
#' 
#' @export
#' 
#' @importFrom R6 R6Class


PairedComparison <- R6Class(
    classname = "PairedComparison",
    inherit = TwoSamplePairedTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `PairedComparison` object. 
        #' 
        #' @template init_params
        #' 
        #' @return A `PairedComparison` object. 
        initialize = function(
            type = c("permu", "approx"),
            alternative = c("two_sided", "less", "greater"), n_permu = NULL
        ) {
            private$.type <- match.arg(type)
            
            super$initialize(alternative = match.arg(alternative), n_permu = n_permu)
        }
    ),
    private = list(
        .name = "Paired Comparison",

        .diff = NULL,

        .calculate_statistic = function() {
            private$.diff <- private$.data$x - private$.data$y

            private$.statistic <- mean(private$.diff)
        },

        .calculate_statistic_permu = function() {
            private$.statistic_permu <- apply(
                X = private$.swapped_permu, MARGIN = 1,
                FUN = function(is_swapped, diff) {
                    mean(diff * (2 * is_swapped - 1))
                }, diff = private$.diff
            )
        },

        .calculate_p = function() {
            z <- private$.statistic / sqrt(
                sum(private$.diff^2) / length(private$.diff)^2
            )

            private$.p_value <- get_p_continous(z, "norm", private$.side)
        }
    )
)