#' @title `r SiegelTukey$private_fields$.name`
#' 
#' @description Performs two sample Siegel-Tukey test on data vectors. 
#' 
#' @aliases twosample.siegel
#' 
#' @export SiegelTukey
#' 
#' @importFrom R6 R6Class


SiegelTukey <- R6Class(
    classname = "SiegelTukey",
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
        }
    ),
    private = list(
        .name = "Siegel-Tukey Test",

        .adjust_median = NULL,

        .trend = "-",

        .calculate_extra = function() {},

        .calculate_p = function() {
            raw_alternative <- private$.alternative
            private$.alternative <- switch(raw_alternative,
                greater = "less", less = "greater", two_sided = "two_sided"
            )

            super$.calculate_p()

            private$.alternative <- raw_alternative
        },

        .calculate_score = function() {
            x <- private$.data$x
            y <- private$.data$y

            if (private$.adjust_median) {
                x <- x - median(x)
                y <- y - median(y)
            }

            c_xy <- c(x, y)
            N <- length(c_xy)

            rank_l <- outer(c(1, 4), seq.int(from = 0, to = N - 1, by = 4), "+")
            rank_r <- outer(c(0, 1), seq.int(from = 2, to = N, by = 4), "+")

            index_floor <- seq_len(floor(N / 2))
            index_ceiling <- seq_len(ceiling(N / 2))
            if (length(rank_l) == length(rank_r)) {
                rank_l <- rank_l[index_floor]
                rank_r <- rank_r[index_ceiling]
            } else {
                rank_l <- rank_l[index_ceiling]
                rank_r <- rank_r[index_floor]
            }

            st_rank <- vapply(
                X = split(c(rank_l, rev(rank_r)), sort(c_xy)),
                FUN = mean, FUN.VALUE = numeric(1)
            )

            private$.data <- list(x = st_rank[as.character(x)], y = st_rank[as.character(y)])
        }
    )
)