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
        #' @template init_params
        #' 
        #' @return A `SiegelTukey` object.
        initialize = function(
            type = c("permu", "asymp", "exact"),
            alternative = c("two_sided", "less", "greater"),
            n_permu = 0L, correct = TRUE
        ) {
            super$.init(
                type = type, alternative = alternative,
                n_permu = n_permu, correct = correct
            )
        }
    ),
    private = list(
        .name = "Siegel-Tukey Test",
        .param_name = "ratio of scales",

        .scoring = "Siegel-Tukey rank",
        .null_value = 1,
        .trend = "-",

        .calculate_score = function() {
            c_xy <- c(private$.data$x, private$.data$y)
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

            rank_xy <- rank(c_xy, ties.method = "first")
            st_rank <- unlist(lapply(
                split(c(rank_l, rev(rank_r)), c_xy[order(rank_xy)]),
                function(x) if ((len <- length(x)) == 1) x else rep.int(mean(x), len)
            ), recursive = FALSE, use.names = FALSE)[rank_xy]

            x_index <- seq_along(private$.data$x)
            private$.data <- list(x = st_rank[x_index], y = st_rank[-x_index])
        },

        .calculate_extra = function() {}
    )
)