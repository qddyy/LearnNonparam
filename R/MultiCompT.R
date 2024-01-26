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
        #' @param method a character string specifying whether to use bonferroni correction.
        #' @param conf_level a numeric value between zero and one giving the family-wise confidence level to use.
        #' 
        #' @return A `MultiCompT` object. 
        initialize = function(
            type = c("permu", "asymp"),
            method = c("bonferroni", "no_bonferroni"),
            scoring = c("none", "rank", "vw", "expon"),
            conf_level = 0.95, n_permu = 0L
        ) {
            self$type <- type
            self$method <- method
            self$scoring <- scoring
            self$conf_level <- conf_level
            self$n_permu <- n_permu
        }
    ),
    private = list(
        .name = "Multiple Comparison Based on t Statistic",

        .define = function() {
            lengths <- lengths(split(private$.data, names(private$.data)))

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

            private$.p_value <- get_p_continous(
                private$.statistic, "t", "lr", df = df
            )
        },

        .calculate_extra = function() {
            private$.differ <- private$.p_value < (1 - private$.conf_level) / (
                if (private$.method == "bonferroni") length(private$.p_value) else 1
            )
        },

        .on_method_change = function() {
            private$.calculate_extra()
        }
    )
)