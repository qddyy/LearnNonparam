#' @title `r KruskalWallis$private_fields$.name`
#' 
#' @description Performs Kruskal-Wallis test on samples.
#' 
#' @aliases ksample.kw
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom stats pchisq


KruskalWallis <- R6Class(
    classname = "KruskalWallis",
    inherit = KSampleTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `KruskalWallis` object.
        #' 
        #' @template init_params
        #' 
        #' @return A `KruskalWallis` object.
        initialize = function(
            type = c("permu", "asymp"),
            scoring = c("rank", "vw", "expon"),
            n_permu = 1e4
        ) {
            self$type <- type
            self$scoring <- scoring
            self$n_permu <- n_permu
        }
    ),
    private = list(
        .name = "Kruskal-Wallis Test",

        .define = function() {
            lengths <- tabulate(as.integer(names(private$.data)))

            mean <- mean(private$.data)
            var <- var(private$.data)

            private$.statistic_func <- function(data, group) {
                sum(lengths * (vapply(
                    X = split.default(data, group), FUN = sum,
                    FUN.VALUE = numeric(1), USE.NAMES = FALSE
                ) / lengths - mean)^2) / var
            }
        },

        .calculate_side = function() {
            private$.side <- "r"
        },

        .calculate_p = function() {
            k <- as.integer(names(private$.data)[length(private$.data)])

            private$.p_value <- get_p_continous(
                private$.statistic, "chisq", "r", df = k - 1
            )
        }
    )
)