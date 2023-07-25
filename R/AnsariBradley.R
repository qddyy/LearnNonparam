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
    inherit = Wilcoxon,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `AnsariBradley` object. 
        #' 
        #' @param ... parameters passed to `Wilcoxon$new()`.
        #' 
        #' @return A `AnsariBradley` object. 
        initialize = function(...) {
            private$.trend <- "-"

            super$initialize(...)
        }
    ),
    private = list(
        .calculate = function() {
            super$.__enclos_env__$super$.calculate()

            m <- length(private$.data$x)
            private$.statistic <- private$.statistic + m * (m + 1) / 2
        },

        .calculate_estimate = function() {},
        .calculate_ci = function() {},

        .calculate_scores = function(data) {
            private$.scoring <- "Ansari-Bradley rank"

            x <- data$x
            y <- data$y

            m <- length(x)
            n <- length(y)
            N <- m + n

            rank <- rank(c(x, y))
            AB_rank <- pmin(rank, N + 1 - rank)

            list(x = AB_rank[1:m], y = AB_rank[(m + 1):(m + n)])
        }
    )
)
