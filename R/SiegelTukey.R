#' @title Siegel-Tukey Test
#' 
#' @description Performs two sample Siegel-Tukey test on data vectors. 
#' 
#' 
#' @export SiegelTukey
#' 
#' @importFrom R6 R6Class


SiegelTukey <- R6Class(
    classname = "Siegel-Tukey Test",
    inherit = Wilcoxon,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `SiegelTukey` object. 
        #' 
        #' @param adjust_median a logical indicating whether the median difference between groups is levelled before the test is conducted. 
        #' 
        #' @param ... extra parameters passed to `Wilcoxon$new()`.
        #' 
        #' @return A `SiegelTukey` object. 
        initialize = function(
            adjust_median = FALSE,
            ...
        ) {
            private$.adjust_median <- adjust_median

            super$initialize(...)

            private$.scoring <- "Siegel-Tukey rank"

            private$.statistic_func <- function(x, y) sum(x)
        }
    ),
    private = list(
        .adjust_median = NULL,

        .trend = "-",

        .calculate_extra = function() {},

        .calculate_p = function() {
            raw_alternative <- private$.alternative
            private$.alternative <- switch(raw_alternative,
                greater = "less", less = "greater", two_sided = "two_sided"
            )

            raw_statistic <- private$.statistic
            m <- length(private$.data$x)
            private$.statistic <- raw_statistic - m * (m + 1) / 2

            super$.calculate_p()

            private$.statistic <- raw_statistic

            private$.alternative <- raw_alternative
        },

        .calculate_scores = function() {
            x <- private$.data$x
            y <- private$.data$y

            if (private$.adjust_median) {
                x <- x - median(x)
                y <- y - median(y)
            }

            m <- length(x)
            n <- length(y)
            N <- m + n

            rank_l <- sapply(
                seq(from = 0, to = N - 1, by = 4),
                function(x) x + c(1, 4)
            ) 
            rank_r <- sapply(
                seq(from = 2, to = N, by = 4),
                function(x) x + c(0, 1)
            )
            if (length(rank_l) == length(rank_r)) {
                rank_l <- rank_l[1:floor(N / 2)]
                rank_r <- rank_r[1:ceiling(N / 2)]
            } else {
                rank_l <- rank_l[1:ceiling(N / 2)]
                rank_r <- rank_r[1:floor(N / 2)]
            }
            ST_rank <- c(rank_l, rev(rank_r))[rank(c(x, y))]

            private$.data <- list(x = ST_rank[1:m], y = ST_rank[(m + 1):(m + n)])
        }
    )
)