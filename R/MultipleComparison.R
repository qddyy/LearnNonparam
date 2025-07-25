#' @title MultipleComparison Class
#' 
#' @description Abstract class for multiple comparisons.
#' 
#' @aliases class.multcomp
#' 
#' @importFrom R6 R6Class
#' @importFrom graphics par layout mtext hist.default abline


MultipleComparison <- R6Class(
    classname = "MultipleComparison",
    inherit = KSampleTest,
    cloneable = FALSE,
    private = list(
        .group_ij = NULL,
        .differ = NULL,

        .preprocess = function() {
            super$.preprocess()

            k <- attr(private$.data, "group")[length(private$.data)]
            private$.group_ij <- list(
                i = rep.int(seq_len(k - 1), seq.int(k - 1, 1)),
                j = unlist(lapply(seq.int(2, k), seq.int, to = k), FALSE, FALSE)
            )
        },

        .calculate_statistic = function() {
            private$.statistic <- multcomp_pmt(
                private$.data,
                attr(private$.data, "group"),
                private$.statistic_func,
                if (private$.type == "permu") private$.n_permu else NA_real_,
                isTRUE(getOption("LearnNonparam.pmt_progress"))
            )
        },

        .calculate_n_permu = function() {
            attr(private$.n_permu, "n_used") <- ncol(
                attr(private$.statistic, "permu")
            )
        },

        .calculate_p_permu = function() {
            statistic_permu <- attr(statistic <- private$.statistic, "permu")

            m <- nrow(statistic_permu)
            n <- ncol(statistic_permu)
            tol <- sqrt(.Machine$double.eps)
            delayedAssign(
                "l", .rowMeans(statistic_permu <= statistic + tol, m, n)
            )
            delayedAssign(
                "r", .rowMeans(statistic_permu >= statistic - tol, m, n)
            )
            delayedAssign(
                "lr", 2 * pmin(l, r, 0.5)
            )

            private$.p_value <- eval(as.name(private$.side))
        },

        .calculate_extra = function() {
            private$.differ <- (
                private$.p_value < 1 - private$.conf_level
            )
        },

        .on_n_permu_change = function() {
            super$.on_n_permu_change()

            private$.calculate_extra()
        },

        .print = function() {
            cat("\n\t", private$.name, "\n\n")

            cat(
                paste("scoring:", private$.scoring),
                paste(
                    "type:",
                    if ((type <- private$.type) == "permu") {
                        n_used <- as.numeric(attr(private$.n_permu, "n_used"))
                        paste0(type, "(", format(n_used), ")")
                    } else type
                ),
                paste("method:", private$.method),
                sep = "    "
            )
            cat("\n\n")

            cat(
                "family-wise confidence level:",
                paste0(format(private$.conf_level * 100), "%")
            )
            cat("\n\n")

            data_names <- names(private$.raw_data)
            print(
                data.frame(
                    statistic = private$.statistic,
                    `p-value` = private$.p_value,
                    ifelse(private$.differ, "*", ""),
                    row.names = paste(
                        data_names[private$.group_ij$i],
                        data_names[private$.group_ij$j],
                        sep = " ~ "
                    ),
                    check.names = FALSE, fix.empty.names = FALSE
                )
            )
        },

        .plot = function(...) {
            original_par <- par(no.readonly = TRUE)
            on.exit(par(original_par))

            n <- attr(private$.data, "group")[length(private$.data)]
            dots <- c(private$.group_ij, list(seq_len(n * (n - 1) / 2)))

            layout_matrix <- matrix(0, n - 1, n - 1)
            .mapply(
                dots = dots, MoreArgs = NULL,
                FUN = function(i, j, k) layout_matrix[j - 1, i] <<- k
            )
            layout(layout_matrix)

            par(oma = c(0, 0, 3, 0))
            .mapply(
                dots = dots, MoreArgs = NULL, FUN = {
                    data_names <- names(private$.raw_data)

                    statistic <- private$.statistic
                    statistic_permu <- attr(statistic, "permu")

                    function(i, j, k) {
                        do_call(
                            func = hist.default,
                            default = list(border = "white"),
                            fixed = list(
                                plot = TRUE,
                                x = statistic_permu[k, ],
                                xlim = bquote(range(breaks, .(statistic[k]))),
                                xlab = "Statistic",
                                main = paste(data_names[i], "~", data_names[j])
                            ), ...
                        )
                        abline(v = statistic[k], lty = "dashed")
                    }
                }
            )
            mtext(
                side = 3, line = 0, outer = TRUE,
                text = expression(bold("Permutation Distribution"))
            )
        },

        .autoplot = function(...) {
            ggplot2::ggplot() +
                do_call(
                    func = ggplot2::stat_bin,
                    default = list(fill = "gray"),
                    fixed = list(
                        geom = "bar",
                        mapping = ggplot2::aes(x = .data$x),
                        data = data.frame(
                            i = rep.int(private$.group_ij$i, private$.n_permu),
                            j = rep.int(private$.group_ij$j, private$.n_permu),
                            x = as.vector(attr(private$.statistic, "permu"))
                        )
                    ), ...
                ) +
                ggplot2::geom_vline(
                    mapping = ggplot2::aes(xintercept = .data$statistic),
                    data = data.frame(
                        i = private$.group_ij$i,
                        j = private$.group_ij$j,
                        statistic = private$.statistic
                    ), linetype = "dashed"
                ) +
                ggplot2::facet_grid(
                    rows = ggplot2::vars(.data$j),
                    cols = ggplot2::vars(.data$i),
                    scales = "free", switch = "both",
                    labeller = {
                        data_names <- names(private$.raw_data)
                        ggplot2::as_labeller(
                            function(index) data_names[as.integer(index)]
                        )
                    }
                ) +
                ggplot2::labs(
                    x = "Statistic", y = "Frequency",
                    title = "Permutation Distribution"
                ) +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(
                        face = "bold", hjust = 0.5
                    )
                )
        }
    )
)