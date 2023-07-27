#' @title Two Sample Permutation Test Based on Correlation Coefficients
#' 
#' @description Performs correlation coefficient based two sample permutation test on data vectors. 
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class


Correlation <- R6Class(
    classname = "Two Sample Permutation Test (correlation coefficient)",
    inherit = TwoSampleAssociationTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `Correlation` object. 
        #' 
        #' @param type a character string specifying the way to calculate p-values, must be one of `"permu"` (default) or `"approx"`. 
        #' @param method a character string indicating which correlation coefficient is to be computed. Must be one of `"pearson"` (default), `"kendall"`, or `"spearman"`. 
        #' 
        #' @param alternative a character string specifying the alternative hypothesis, must be one of `"two_sided"` (default), `"greater"` or `"less"`.
        #' @param n_permu an integer specifying how many permutations should be used to construct the permutation distribution. If `NULL` (default) then all permutations are used.
        #' 
        #' @return A `Correlation` object. 
        initialize = function(
            type = c("permu", "approx"), method = c("pearson", "kendall", "spearman"),
            alternative = c("two_sided", "less", "greater"), n_permu = NULL
        ) {
            private$.type <- match.arg(type)
            private$.method <- match.arg(method)

            super$initialize(alternative = match.arg(alternative), n_permu = n_permu)
        }
    ),
    private = list(
        .calculate = function() {
            private$.statistic_func <- switch(
                private$.method,
                pearson = function(x, y) cor(x, y, method = "pearson"),
                kendall = function(x, y) cor(x, y, method = "kendall"),
                spearman = function(x, y) cor(x, y, method = "spearman")
            )

            super$.calculate()
        },

        .calculate_p = function() {
            n <- nrow(private$.data)

            if (private$.method == "kendall") {
                x <- private$.data$x
                y <- private$.data$y

                # modified stats:::cor.test.default
                x_ties <- table(x[duplicated(x)]) + 1
                y_ties <- table(y[duplicated(y)]) + 1
                T_0 <- n * (n - 1) / 2
                T_1 <- sum(x_ties * (x_ties - 1)) / 2
                T_2 <- sum(y_ties * (y_ties - 1)) / 2
                S <- private$.statistic * sqrt((T_0 - T_1) * (T_0 - T_2))
                v_0 <- n * (n - 1) * (2 * n + 5)
                v_t <- sum(x_ties * (x_ties - 1) * (2 * x_ties + 5))
                v_u <- sum(y_ties * (y_ties - 1) * (2 * y_ties + 5))
                v_1 <- sum(x_ties * (x_ties - 1)) * sum(y_ties * (y_ties - 1))
                v_2 <- sum(x_ties * (x_ties - 1) * (x_ties - 2)) * sum(y_ties * (y_ties - 1) * (y_ties - 2))
                var_S <- (v_0 - v_t - v_u) / 18 + v_1 / (2 * n * (n - 1)) + v_2 / (9 * n * (n - 1) * (n - 2))
                
                z <- S / sqrt(var_S)
            } else {
                z <- private$.statistic * sqrt(n - 1)
            }

            less <- pnorm(z)
            greater <- pnorm(z, lower.tail = FALSE)
            two_sided <- 2 * min(less, greater)

            private$.p_value <- switch(private$.alternative,
                greater = greater, less = less, two_sided = two_sided
            )
        }
    )
)