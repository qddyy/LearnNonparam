#' @title Multiple Comparison Class
#' 
#' @description This class specializes `KSampleTest` for multiple comparisons. 
#' 
#' 
#' @export
#' 
#' @import ggplot2
#' @importFrom R6 R6Class
#' @importFrom arrangements combinations


MultipleComparison <- R6Class(
    classname = "Multiple Comparison",
    inherit = KSampleTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `MultipleComparison` object. Note that it is not recommended to create objects of this class directly. 
        #' 
        #' @param conf_level a numeric value between zero and one giving the family-wise confidence level to use. 
        #' @param n_permu an integer specifying how many permutations should be used to construct the permutation distribution. If `NULL` (default) then all permutations are used. 
        #' @param scoring a character string specifying which scoring system to be used, must be one of `"none"` (default), `"rank`, `"vw"` or `"expon"`. 
        #' 
        #' @return A `MultipleComparison` object. 
        initialize = function(conf_level = 0.95, n_permu = NULL, scoring = c("none", "rank", "vw", "expon")) {
            super$initialize(alternative = "two_sided", conf_level = conf_level, n_permu = n_permu, scoring = match.arg(scoring))
        }
    ),
    private = list(
        .multicomp = NULL,

        .c_groups = NULL,

        .check = function() {}, # TODO

        .print = function(digits) {
            cat("\n")
            cat(strwrap(class(self)[1], prefix = "\t"), sep = "\n")
            cat("\n")

            cat(
                "family-wise confidence level:",
                format(100 * private$.conf_level, digits = 2), "(%)"
            )
            cat("\n\n")

            print(private$.multicomp, digits = digits, row.names = FALSE)
        },

        .plot = function(...) {
            histograms <- ggplot() +
                stat_bin(
                    data = do.call(
                        rbind, .mapply(
                            dots = list(
                                private$.c_groups,
                                split(
                                    private$.statistic_permu,
                                    row(private$.statistic_permu)
                                )
                            ),
                            FUN = function(ij, statistic_permu) {
                                ij <- as.integer(ij)
                                data.frame(
                                    i = ij[1], j = ij[2],
                                    statistic_permu = statistic_permu
                                )
                            },
                            MoreArgs = NULL
                        )
                    ),
                    mapping = aes(x = statistic_permu),
                    geom = "bar", fill = "#68aaa1"
                ) +
                geom_vline(
                    data = private$.multicomp[1:3],
                    mapping = aes(xintercept = statistic),
                    linetype = "dashed"
                ) +
                facet_grid(j ~ i, scales = "free", switch = "both")
            print(histograms)
        },

        .calculate_statistic = function() {
            private$.c_groups <- combinations(
                v = unique(names(private$.data)), k = 2, layout = "list"
            )

            data <- unname(private$.data)
            statistic_func <- private$.statistic_func
            private$.statistic_func <- function(group) {
                group_loc <- split(seq_along(group), group)
                vapply(
                    X = private$.c_groups, FUN.VALUE = numeric(1),
                    FUN = function(ij) {
                        statistic_func(
                            data[group_loc[[ij[1]]]],
                            data[group_loc[[ij[2]]]],
                            setNames(data, group)
                        )
                    }
                )
            }

            private$.statistic <- private$.statistic_func(names(private$.data))
        },

        .calculate_statistic_permu = function() {
            private$.statistic_permu <- vapply(
                X = private$.group_permu, FUN = private$.statistic_func,
                FUN.VALUE = numeric(length(private$.c_groups))
            )
        },

        .calculate_p_permu = function() {
            private$.p_value <- rowMeans(
                abs(private$.statistic_permu) >= abs(private$.statistic)
            )
        },

        .calculate_extra = function() {
            private$.multicomp <- setNames(
                as.data.frame(cbind(
                    t(vapply(private$.c_groups, as.integer, integer(2))),
                    private$.statistic, private$.p_value
                )), c("i", "j", "statistic", "p")
            )

            private$.multicomp$differ <- (private$.p_value < 1 - private$.conf_level)
        }
    )
)