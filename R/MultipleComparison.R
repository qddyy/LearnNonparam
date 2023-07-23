#' @title Multiple Comparison Class
#' 
#' @description This class specializes `KSampleTest` for multiple comparisons. 
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom arrangements combinations


MultipleComparison <- R6Class(
    classname = "Multiple Comparison",
    inherit = KSampleTest,
    cloneable = FALSE,
    public = list(
        #' @description Create a new `MultipleComparison` object. Note that it is not recommended to create objects of this class directly. 
        #' 
        #' @param signif_level a numeric value between zero and one giving the significance level.
        #' @param ... extra parameters passed to `PermuTest$initialize`.
        #' 
        #' @return A `MultipleComparison` object. 
        initialize = function(signif_level = 0.05, ...) {
            private$.signif_level <- signif_level

            super$initialize(alternative = "two_sided", ...)
        }
    ),
    private = list(
        .groups = NULL,
        .c_groups = NULL,

        .signif_level = NULL,
        .multicomp = NULL,

        .check = function() {}, # TODO

        .calculate = function(...) {
            private$.groups <- unique(names(private$.data))
            private$.c_groups <- combinations(v = private$.groups, k = 2)

            super$.calculate()

            private$.prepare_multicomp()
        },

        .calculate_statistic = function() {
            group <- names(private$.data)
            g_index <- setNames(lapply(
                private$.groups, function(g) which(group == g)
            ), private$.groups)

            statistic_func <- private$.statistic_func
            data <- unname(private$.data)
            private$.statistic <- apply(
                private$.c_groups, 1,
                function(ij) statistic_func(
                    data[g_index[[ij[1]]]], data[g_index[[ij[2]]]], setNames(data, group)
                )
            )
        },

        .calculate_statistic_permu = function() {
            groups <- private$.groups
            statistic_func <- private$.statistic_func
            data <- unname(private$.data)
            private$.statistic_permu <- sapply(
                private$.group_permu, function(group) {
                    g_index <- setNames(lapply(
                        groups, function(g) which(group == g)
                    ), groups)
                    apply(
                        private$.c_groups, 1,
                        function(ij) statistic_func(
                            data[g_index[[ij[1]]]], data[g_index[[ij[2]]]], setNames(data, group)
                        )
                    )
                }
            )
        },

        .calculate_p_permu = function() {
            private$.p_value <- rowMeans(
                abs(private$.statistic_permu) >= abs(private$.statistic)
            )
        },

        .prepare_multicomp = function() {
            private$.multicomp <- setNames(as.data.frame(cbind(
                apply(private$.c_groups, 2, as.integer),
                private$.statistic, private$.p_value
            )), c("i", "j", "statistic", "p"))

            private$.multicomp$differ <- private$.p_value < private$.signif_level
        }
    ),
    active = list(
        #' @field signif_level The significance level. 
        signif_level = function() {
            if (missing(value)) {
                private$.signif_level
            } else {
                private$.signif_level <- value
                private$.check()
                private$.prepare_multicomp()
            }
        },
        #' @field multicomp The multiple comparison result. 
        multicomp = function() private$.multicomp
    )
)