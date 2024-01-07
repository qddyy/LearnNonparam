#' @title MultipleComparison Class
#' 
#' @description This class specializes `KSampleTest` for multiple comparisons. Note that it is not recommended to create objects of this class directly. 
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom graphics par layout mtext hist abline


MultipleComparison <- R6Class(
    classname = "MultipleComparison",
    inherit = KSampleTest,
    cloneable = FALSE,
    private = list(
        .name = "Multiple Comparison",

        .group_ij = NULL,
        .multicomp = NULL,

        .check = function() {},

        .preprocess = function(...) {
            super$.preprocess(...)

            k <- as.integer(get_last(names(private$.data)))
            private$.group_ij <- list(
                i = rep.int(seq_len(k - 1), seq.int(k - 1, 1)),
                j = unlist(lapply(
                    seq.int(2, k), seq.int, to = k
                ), recursive = FALSE, use.names = FALSE)
            )
        },

        .calculate_statistic = function() {
            data <- unname(private$.data)
            group <- as.integer(names(private$.data))
            private$.statistic <- as.numeric(.mapply(
                FUN = function(i, j) {
                    private$.statistic_func(i, j, data, group)
                }, dots = private$.group_ij, MoreArgs = NULL
            ))
        },

        .calculate_statistic_permu = function() {
            private$.statistic_permu <- multicomp_pmt(
                group_i = private$.group_ij$i,
                group_j = private$.group_ij$j,
                data = unname(private$.data),
                group = as.integer(names(private$.data)),
                statistic_func = private$.statistic_func,
                n_permu = private$.n_permu
            )
        },

        .calculate_p_permu = function() {
            private$.p_value <- rowMeans(
                abs(private$.statistic_permu) >= abs(private$.statistic)
            )
        },

        .calculate_extra = function() {
            private$.multicomp <- data.frame(
                group_i = private$.group_ij$i,
                group_j = private$.group_ij$j,
                statistic = private$.statistic,
                p_value = private$.p_value,
                differ = (private$.p_value < 1 - private$.conf_level)
            )
        },

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
                sep = "    "
            )
            cat("\n\n")

            cat(
                "family-wise confidence level:",
                paste0(
                    format(private$.conf_level * 100, digits = digits), "%"
                )
            )
            cat("\n\n")

            data_names <- names(private$.raw_data)
            multicomp <- private$.multicomp
            multicomp$group_i <- data_names[multicomp$group_i]
            multicomp$group_j <- data_names[multicomp$group_j]
            print(multicomp, digits = digits, row.names = FALSE)
        },

        .plot = function(...) {
            n <- get_last(private$.group_ij$i)

            dots <- c(private$.group_ij, list(seq_len(n * (n + 1) / 2)))

            layout_matrix <- matrix(0, n, n)
            .mapply(
                FUN = function(i, j, k) {
                    layout_matrix[j - 1, i] <<- k
                }, dots = dots, MoreArgs = NULL
            )

            defaut_par <- par(no.readonly = TRUE)
            par(oma = c(0, 0, 3, 0))
            layout(layout_matrix)

            data_names <- names(private$.raw_data)
            .mapply(
                FUN = function(i, j, k) {
                    do_call(
                        func = hist,
                        fixed = list(
                            x = private$.statistic_permu[k, ],
                            plot = TRUE,
                            xlab = "Statistic",
                            main = paste(data_names[i], "versus", data_names[j])
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
                                group_i = rep.int(private$.group_ij$i, n),
                                group_j = rep.int(private$.group_ij$j, n),
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
                    rows = ggplot2::vars(.data$group_j),
                    cols = ggplot2::vars(.data$group_i),
                    scales = "free", switch = "both",
                    labeller = {
                        data_names <- names(private$.raw_data)
                        ggplot2::as_labeller(
                            function(index) data_names[as.integer(index)]
                        )
                    }
                ) +
                ggplot2::labs(
                    title = "Permutation Distribution",
                    x = "Statistic", y = "Count"
                ) +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
                )
        }
    )
)