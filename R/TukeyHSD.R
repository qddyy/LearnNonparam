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
#' @importFrom graphics hist abline


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
            scoring = c("none", "rank", "vw", "expon"),
            conf_level = 0.95, n_permu = 0L
        ) {
            private$.init(
                type = type, scoring = scoring,
                conf_level = conf_level, n_permu = n_permu
            )
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
                    abs(means[i] - means[j]) / sqrt(
                        mse / 2 * (1 / lengths[i] + 1 / lengths[j])
                    )
                }
            } else {
                var <- var(private$.data)
                private$.statistic_func <- function(i, j, data, group) {
                    abs(mean(data[group == i]) - mean(data[group == j])) / sqrt(
                        var / 2 * (1 / lengths[i] + 1 / lengths[j])
                    )
                }
            }
        },

        .calculate_statistic_permu = function() {
            super$.calculate_statistic_permu()

            private$.statistic_permu <- apply(private$.statistic_permu, 2, max)
        },

        .calculate_n_permu = function() {
            length(private$.statistic_permu)
        },

        .calculate_p_permu = function() {
            private$.p_value <- rowMeans(
                outer(private$.statistic, private$.statistic_permu, `<=`)
            )
        },

        .calculate_p = function() {
            N <- length(private$.data)
            k <- as.integer(names(private$.data)[N])
            df <- if (private$.scoring == "none") N - k else Inf

            private$.p_value <- get_p_continous(
                private$.statistic, "tukey", "r", nmeans = k, df = df
            )
        },

        .plot = function(...) {
            do_call(
                func = hist,
                default = list(border = "white"),
                fixed = list(
                    x = private$.statistic_permu,
                    plot = TRUE,
                    xlab = "Statistic",
                    main = "Permutation Distribution"
                ), ...
            )
            abline(
                v = private$.statistic, lty = "dashed",
                col = seq_along(private$.statistic)
            )
            legend(
                x = "topright", lty = "dashed",
                col = seq_along(private$.statistic),
                legend = unlist(.mapply(
                    dots = private$.group_ij, FUN = {
                        data_names <- names(private$.raw_data)
                        function(i, j) {
                            paste(data_names[i], data_names[j], sep = " ~ ")
                        }
                    }, MoreArgs = NULL
                ))
            )
        },

        .autoplot = function(...) {
            ggplot2::ggplot() +
                do_call(
                    func = ggplot2::stat_bin,
                    default = list(fill = "gray"),
                    fixed = list(
                        geom = "bar",
                        mapping = ggplot2::aes(y= .data$statistic),
                        data = data.frame(statistic = private$.statistic_permu)
                    ), ...
                ) +
                ggplot2::geom_hline(
                    mapping = ggplot2::aes(
                        yintercept = .data$statistic, color = .data$pair
                    ),
                    data = data.frame(
                        statistic = private$.statistic,
                        pair = {
                            data_names <- names(private$.raw_data)
                            unlist(.mapply(
                                dots = private$.group_ij, FUN = function(i, j) {
                                    paste(data_names[i], "~", data_names[j])
                                }, MoreArgs = NULL
                            ))
                        }
                    ), linetype = "dashed"
                ) +
                ggplot2::coord_flip() +
                ggplot2::labs(
                    title = "Permutation Distribution",
                    y = "Statistic", x = "Frequency"
                ) +
                ggplot2::theme(
                    legend.position = c(1, 1),
                    legend.justification = c(1, 1),
                    legend.title = ggplot2::element_blank(),
                    plot.title = ggplot2::element_text(
                        face = "bold", hjust = 0.5
                    )
                )
        }
    )
)