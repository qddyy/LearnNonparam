#' @title `r TukeyHSD$private_fields$.name`
#' 
#' @description Performs Tukey's honest significant difference method on data vectors. Procedure modified by Kramer is performed for unequal sample sizes.
#' 
#' @aliases multicomp.tukey
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom stats ptukey


TukeyHSD <- R6Class(
    classname = "TukeyHSD",
    inherit = MultipleComparison,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `TukeyHSD` object.
        #' 
        #' @template init_params
        #' @param conf_level a numeric value between zero and one giving the family-wise confidence level to use.
        #' 
        #' @return A `TukeyHSD` object.
        initialize = function(
            type = c("permu", "asymp"),
            conf_level = 0.95, n_permu = 0L, scoring = c("none", "rank", "vw", "expon")
        ) {
            private$.type <- match.arg(type)

            super$initialize(conf_level = conf_level, n_permu = n_permu, scoring = match.arg(scoring))
        }
    ),
    private = list(
        .name = "Tukey's HSD",

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
                        mse / 2 * (1 / lengths[i] + 1 / lengths[j])
                    )
                }
            } else {
                var <- var(private$.data)
                private$.statistic_func <- function(i, j, data, group) {
                    (mean(data[group == i]) - mean(data[group == j])) / sqrt(
                        var / 2 * (1 / lengths[i] + 1 / lengths[j])
                    )
                }
            }
        },

        .calculate_p_permu = function() {
            Q_star <- apply(abs(private$.statistic_permu), 2, max)

            private$.p_value <- vapply(
                X = abs(private$.statistic), FUN.VALUE = numeric(1),
                FUN = function(abs_T_ij) mean(Q_star >= abs_T_ij)
            )
        },

        .calculate_p = function() {
            N <- length(private$.data)
            k <- as.integer(names(private$.data)[N])
            df <- if (private$.scoring == "none") N - k else Inf

            private$.p_value <- get_p_continous(
                abs(private$.statistic), "tukey", "r", nmeans = k, df = df
            )
        }
    )
)