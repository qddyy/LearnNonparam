#' @title `r KruskalWallis$private_fields$.name`
#' 
#' @description Performs k sample Kruskal-Wallis Test on data vectors. 
#' 
#' @aliases ksample.kw
#' 
#' @export
#' 
#' @importFrom R6 R6Class


KruskalWallis <- R6Class(
    classname = "KruskalWallis",
    inherit = KSampleTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `KruskalWallis` object. 
        #' 
        #' @param type a character string specifying the way to calculate p-values, must be one of `"permu"` (default) or `"approx"`. 
        #' 
        #' @template init_params
        #' 
        #' @return A `KruskalWallis` object. 
        initialize = function(
            type = c("permu", "approx"),
            n_permu = NULL, scoring = c("rank", "vw", "expon")
        ) {
            private$.type <- match.arg(type)

            super$initialize(scoring = match.arg(scoring), alternative = "greater", n_permu = n_permu)
        }
    ),
    private = list(
        .name = "Kruskal-Wallis Test",

        .define_statistic = function() {
            mean <- mean(private$.data)
            var <- var(private$.data)
            private$.statistic_func <- function(data, group) {
                sum(vapply(
                    X = split(data, group),
                    FUN = function(x) length(x) * (mean(x) - mean)^2,
                    FUN.VALUE = numeric(1), USE.NAMES = FALSE
                )) / var
            }
        },

        .calculate_p = function() {
            k <- as.integer(get_last(names(private$.data)))

            private$.p_value <- get_p_continous(
                private$.statistic, "chisq", "r", df = k - 1
            )
        }
    )
)