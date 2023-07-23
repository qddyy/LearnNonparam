#' @title Ansari-Bradley Test
#' 
#' @description Performs two sample Ansari-Bradley test on data vectors. 
#' 
#' 
#' @export AnsariBradley
#' 
#' @importFrom R6 R6Class


AnsariBradley <- R6Class(
    classname = "Ansari-Bradley Test",
    inherit = TwoSampleTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `AnsariBradley` object. 
        #' 
        #' @param alternative a character string specifying the alternative hypothesis, must be one of `"two_sided"` (default), `"greater"` or `"less"`.
        #' @param n_permu an integer specifying how many permutations should be used to construct the permutation distribution. If `NULL` (default) then all permutations are used.
        #' 
        #' @return A `AnsariBradley` object. 
        initialize = function(alternative = c("two_sided", "less", "greater"), n_permu = NULL) {
            super$initialize(alternative = match.arg(alternative), n_permu = n_permu)

            private$.statistic_func <- sum
            private$.trend <- "-"
        }
    ),
    private = list(
        .calculate_scores = function(data) {
            AnsariBradley_rank(data$x, data$y)
        }
    )
)


# modified exactRankTests::cscores.default
AnsariBradley_rank = function(x, y) {
    m <- length(x)
    n <- length(y)

    rank <- rank(c(x, y))
    AB_rank <- pmin(rank, length(rank) - rank + 1)

    list(x = AB_rank[1:m], y = AB_rank[(m + 1):(m + n)])
}