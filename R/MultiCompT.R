#' @title `r MultiCompT$private_fields$.name`
#' 
#' @description Performs t statistic based multiple comparison on data vectors. 
#' 
#' @aliases multicomp.t
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom stats pt


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
            type = c("permu", "asymp"), bonferroni = TRUE,
            conf_level = 0.95, n_permu = 0L, scoring = c("none", "rank", "vw", "expon")
        ) {
            private$.type <- match.arg(type)
            private$.bonferroni <- bonferroni
            
            super$initialize(conf_level = conf_level, n_permu = n_permu, scoring = match.arg(scoring))
        }
    ),
    private = list(
        .name = "Multiple Comparison Based on t Statistic",

        .bonferroni = NULL,

        .define = function() {
            lengths <- vapply(
                X = split(private$.data, names(private$.data)),
                FUN = length, FUN.VALUE = integer(1), USE.NAMES = FALSE
            )

            if (private$.scoring == "none") {
                N <- length(private$.data)
                k <- as.integer(names(private$.data)[N])
                private$.statistic_func <- function(i, j, data, group) {
                    means <- vapply(
                        X = split(data, group), FUN = mean,
                        FUN.VALUE = numeric(1), USE.NAMES = FALSE
                    )
                    mse <- sum((data - means[group])^2) / (N - k)
                    (means[i] - means[j]) / sqrt(
                        mse * (1 / lengths[i] + 1 / lengths[j])
                    )
                }
            } else {
                var <- var(private$.data)
                private$.statistic_func <- function(i, j, data, group) {
                    (mean(data[group == i]) - mean(data[group == j])) / sqrt(
                        var * (1 / lengths[i] + 1 / lengths[j])
                    )
                }
            }
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