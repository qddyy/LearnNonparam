#' @title `r MultiCompT$private_fields$.name`
#' 
#' @description Performs t statistic based multiple comparison on data vectors. 
#' 
#' @aliases multicomp.t
#' 
#' @export
#' 
#' @importFrom R6 R6Class


MultiCompT <- R6Class(
    classname = "MultiCompT",
    inherit = MultipleComparison,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `MultiCompT` object. 
        #' 
        #' @template init_params
        #' @param conf_level a numeric value between zero and one giving the family-wise confidence level to use. 
        #' @param bonferroni a logical indicating whether to apply bonferroni adjustment. 
        #' 
        #' @return A `MultiCompT` object. 
        initialize = function(
            type = c("permu", "approx"), bonferroni = TRUE,
            conf_level = 0.95, n_permu = NULL, scoring = c("none", "rank", "vw", "expon")
        ) {
            private$.type <- match.arg(type)
            private$.bonferroni <- bonferroni
            
            super$initialize(conf_level = conf_level, n_permu = n_permu, scoring = match.arg(scoring))
        }
    ),
    private = list(
        .name = "Multiple Comparison Based on t Statistic",

        .bonferroni = NULL,

        .define_statistic = function() {
            if (private$.scoring == "none") {
                N <- length(private$.data)
                k <- as.integer(names(private$.data)[N])
                private$.statistic_func <- function(x, y, data) {
                    mse <- sum(vapply(
                        X = split(data, names(data)),
                        FUN = function(x) (length(x) - 1) * var(x),
                        FUN.VALUE = numeric(1), USE.NAMES = FALSE
                    )) / (N - k)
                    (mean(x) - mean(y)) / sqrt(
                        mse * (1 / length(x) + 1 / length(y))
                    )
                }
            } else {
                var <- var(private$.data)
                private$.statistic_func <- function(x, y, data) {
                    (mean(x) - mean(y)) / sqrt(
                        var * (1 / length(x) + 1 / length(y))
                    )
                }
            }

            super$.define_statistic()
        },

        .calculate_p = function() {
            N <- length(private$.data)
            k <- as.integer(names(private$.data)[N])
            df <- if (private$.scoring == "none") N - k else Inf

            private$.p_value <- 2 * get_p_continous(
                abs(private$.statistic), "t", "r", df = df
            )
        },

        .calculate_extra = function() {
            super$.calculate_extra()

            if (private$.bonferroni) {
                private$.multicomp$differ <- (
                    private$.p_value < (1 - private$.conf_level) / nrow(private$.multicomp)
                )
            }
        }
    )
)