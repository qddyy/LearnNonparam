#' @title K Sample Permutation Test Based on F Statistic
#' 
#' @description Performs F statistic based k sample permutation test on data vectors. 
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class


ANOVA <- R6Class(
    classname = "ANOVA",
    inherit = KSampleTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `ANOVA` object. 
        #' 
        #' @param type a character string specifying the way to calculate p-values, must be one of `"permu"` (default) or `"approx"`. 
        #' 
        #' @param n_permu an integer specifying how many permutations should be used to construct the permutation distribution. If `NULL` (default) then all permutations are used.
        #' 
        #' @return A `ANOVA` object. 
        initialize = function(
            type = c("permu", "approx"),
            n_permu = NULL
        ) {
            private$.type <- match.arg(type)

            super$initialize(alternative = "greater", n_permu = n_permu)
        }
    ),
    private = list(
        .calculate_statistic = function() {
            private$.statistic_func <- switch(private$.type,
                permu = function(data, group) sum(tapply(
                    data, group, function(x) length(x) * mean(x)^2
                )),
                approx = function(data, group) anova(lm(data ~ as.factor(group)))$F[1]
            )

            super$.calculate_statistic()
        },

        .calculate_p = function() {
            N <- length(private$.data)
            k <- as.integer(names(private$.data)[N])

            private$.p_value <- pf(
                private$.statistic, df1 = k - 1, df2 = N - k, , lower.tail = FALSE
            )
        }
    )
)