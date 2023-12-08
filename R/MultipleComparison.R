#' @title MultipleComparison Class
#' 
#' @description This class specializes `KSampleTest` for multiple comparisons. Note that it is not recommended to create objects of this class directly. 
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class


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
            cat("\n\t", private$.name, "\n\n")

            cat(
                paste("scoring:", private$.scoring),
                paste(
                    "type:",
                    if ((type <- private$.type) == "permu") {
                        n <- as.numeric(ncol(private$.statistic_permu))
                        paste0(type, "(", format(n, digits = digits), ")")
                    } else type
                ),
                paste("method:", private$.method),
                "\n\n", sep = "    "
            )

            cat(sprintf(
                "family-wise confidence level: %.0f%% \n\n",
                private$.conf_level * 100
            ))

            print(private$.multicomp, digits = digits, row.names = FALSE)
        },

        .plot = function(...) {
            n <- get_last(private$.ij$i)

            dots <- c(private$.ij, list(seq_len(n * (n + 1) / 2)))

            layout_matrix <- matrix(0, n, n)
            .mapply(
                FUN = function(i, j, k) {
                    layout_matrix[j - 1, i] <<- k
                }, dots = dots, MoreArgs = NULL
            )

            defaut_par <- par(no.readonly = TRUE)
            par(oma = c(0, 0, 3, 0))
            layout(layout_matrix)
            .mapply(
                FUN = function(i, j, k) {
                    do_call(
                        func = hist,
                        fixed = list(
                            x = private$.statistic_permu[k, ],
                            plot = TRUE,
                            xlab = "Statistic",
                            main = paste(i, "versus", j)
                        ), ...
                    )
                    abline(v = private$.statistic[k], lty = "dashed")
                }, dots = dots, MoreArgs = NULL
            )
            mtext(
                text = expression(bold("Permutation Distribution")),
                side = 3, line = 0, outer = TRUE
            )
            par(defaut_par)
        },

        .autoplot = function(...) {
            ggplot2::ggplot() +
                do_call(
                    func = ggplot2::stat_bin,
                    default = list(fill = "#68aaa1"),
                    fixed = list(
                        geom = "bar",
                        mapping = ggplot2::aes(x = .data$statistic),
                        data = {
                            n <- ncol(private$.statistic_permu)
                            data.frame(
                                i = rep.int(private$.ij$i, n),
                                j = rep.int(private$.ij$j, n),
                                statistic = as.vector(private$.statistic_permu)
                            )
                        }
                    ), ...
                ) +
                ggplot2::geom_vline(
                    data = private$.multicomp[1:3],
                    mapping = ggplot2::aes(xintercept = statistic),
                    linetype = "dashed"
                ) +
                ggplot2::facet_grid(
                    rows = ggplot2::vars(.data$j),
                    cols = ggplot2::vars(.data$i),
                    scales = "free", switch = "both"
                ) +
                ggplot2::labs(
                    title = "Permutation Distribution",
                    x = "Statistic", y = "Count"
                ) +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
                )
        },

        .input = function(...) {
            super$.input(...)

            k <- as.integer(get_last(names(private$.raw_data)))
            private$.ij <- list(
                i = rep.int(seq_len(k - 1), seq.int(k - 1, 1)),
                j = unlist(lapply(
                    seq.int(2, k), seq.int, to = k
                ), recursive = FALSE, use.names = FALSE)
            )
        },

        .calculate_statistic = function() {
            data <- unname(private$.data)
            group <- as.integer(names(private$.data))
            where <- split(seq_along(group), group)
            private$.statistic <- as.numeric(.mapply(
                FUN = function(i, j) {
                    private$.statistic_func(
                        data[where[[i]]], data[where[[j]]], data, group
                    )
                }, dots = private$.ij, MoreArgs = NULL
            ))
        },

        .calculate_statistic_permu = function() {
            private$.statistic_permu <- multicomp_pmt(
                group_i = private$.ij$i - 1,
                group_j = private$.ij$j - 1,
                data = unname(private$.data),
                group = as.integer(names(private$.data)),
                statistic_func = private$.statistic_func,
                n_permu = as.integer(private$.n_permu)
            )
        },

        .calculate_p_permu = function() {
            private$.p_value <- rowMeans(
                abs(private$.statistic_permu) >= abs(private$.statistic)
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