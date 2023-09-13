#' @title MultipleComparison Class
#' 
#' @description This class specializes `KSampleTest` for multiple comparisons. Note that it is not recommended to create objects of this class directly. 
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom ggplot2 ggplot aes stat_bin geom_vline facet_grid labs theme element_text


MultipleComparison <- R6Class(
    classname = "MultipleComparison",
    inherit = KSampleTest,
    cloneable = FALSE,
    private = list(
        .name = "Multiple Comparison",

        .ij = NULL,
        .multicomp = NULL,

        .check = function() {},

        .print = function(digits) {
            cat("\n", "\t", private$.name, "\n\n")

            cat(sprintf(
                "family-wise confidence level: %.0f%% \n\n",
                private$.conf_level * 100
            ))

            cat(
                paste("scoring:", private$.scoring),
                paste("type:", private$.type),
                paste("method:", private$.method),
                "\n\n", sep = "    "
            )

            print(private$.multicomp, digits = digits, row.names = FALSE)
        },

        .plot = function(...) {
            histograms <- ggplot() +
                stat_bin(
                    data = do.call(
                        rbind, .mapply(
                            dots = list(
                                i = private$.ij$i,
                                j = private$.ij$j,
                                statistic_permu = split(
                                    private$.statistic_permu,
                                    row(private$.statistic_permu)
                                )
                            ), FUN = data.frame, MoreArgs = NULL
                        )
                    ),
                    mapping = aes(x = statistic_permu),
                    geom = "bar", fill = "#68aaa1", ...
                ) +
                geom_vline(
                    data = private$.multicomp[1:3],
                    mapping = aes(xintercept = statistic),
                    linetype = "dashed"
                ) +
                facet_grid(j ~ i, scales = "free", switch = "both") +
                labs(
                    title = "Permutation Distribution",
                    x = "Statistic", y = "Count"
                ) +
                theme(plot.title = element_text(face = "bold", hjust = 0.5))
            print(histograms)
        },

        .define_statistic = function() {
            k <- as.integer(get_last(names(private$.data)))
            private$.ij <- ij <- list(
                i = rep.int(seq_len(k - 1), seq.int(k - 1, 1)),
                j = c(lapply(seq.int(2, k), seq.int, to = k), recursive = TRUE)
            )

            data <- unname(private$.data)
            statistic_func <- private$.statistic_func
            private$.statistic_func <- function(group) {
                where <- split(seq_along(group), group)
                as.numeric(.mapply(
                    FUN = function(i, j) {
                        statistic_func(
                            data[where[[i]]],
                            data[where[[j]]],
                            setNames(data, group)
                        )
                    }, dots = ij, MoreArgs = NULL
                ))
            }
        },

        .calculate_statistic = function() {
            private$.statistic <- private$.statistic_func(
                as.integer(names(private$.data))
            )
        },

        .calculate_statistic_permu = function() {
            private$.statistic_permu <- get_arrangement(
                "permute", n_sample = private$.n_permu,
                v = as.integer(names(private$.data)),
                func = private$.statistic_func,
                func_value = numeric(length(private$.ij$i))
            )
        },

        .calculate_p_permu = function() {
            private$.p_value <- rowMeans(
                abs(t(private$.statistic_permu)) >= abs(private$.statistic)
            )
        },

        .calculate_extra = function() {
            private$.multicomp <- data.frame(
                i = private$.ij$i,
                j = private$.ij$j,
                statistic = private$.statistic,
                p_value = private$.p_value,
                differ = (private$.p_value < 1 - private$.conf_level)
            )
        }
    )
)