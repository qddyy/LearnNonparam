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
        #' @param alternative a character string specifying the alternative hypothesis, must be one of `"two_sided"` (default), `"greater"` or `"less"`.
        #' 
        #' @return A `SiegelTukey` object. 
        initialize = function(
            adjust_median = FALSE,
            ...,
            alternative = c("two_sided", "less", "greater")
        ) {
            private$.adjust_median <- adjust_median

            super$initialize(
                ...,
                alternative = match.arg(alternative)
            )
        }
    ),
    private = list(
        .adjust_median = NULL,

        .calculate_scores = function(data) {
            if (private$.adjust_median) {
                data$x <- data$x - median(data$x)
                data$y <- data$y - median(data$y)
            }

            super$.calculate_scores(SiegelTukey_rank(data$x, data$y))
        }
    )
)

# modified jmuOutlier::siegel.test
SiegelTukey_rank <- function(x, y) {
    z <- sort(c(x, y))
    N <- length(z)
    lower <- 1
    upper <- N
    j <- rep(NA, N)
    for (i in 1:N) {
        if (i - 4 * as.integer(i/4) <= 1) {
            j[lower] <- i
            lower <- lower + 1
        }
        else {
            j[upper] <- i
            upper <- upper - 1
        }
    }
    z_0 <- z[order(j)]
    rank.x.mat <- matrix(NA, length(x), N)
    rank.y.mat <- matrix(NA, length(y), N)
    for (k in 1:N) {
        for (i in 1:length(x)) {
            if (x[i] == z_0[k]) {
                rank.x.mat[i, k] <- k
            }
        }
        for (i in 1:length(y)) {
            if (y[i] == z_0[k]) {
                rank.y.mat[i, k] <- k
            }
        }
    }

    list(
        x = apply(rank.x.mat, 1, mean, na.rm = TRUE),
        y = apply(rank.y.mat, 1, mean, na.rm = TRUE)
    )
}
