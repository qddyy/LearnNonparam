#' @title PermuTest Class
#' 
#' @description This is the abstract base class for permutation test objects, which contains several scoring systems. 
#' 
#' 
#' @export
#' 
#' @import ggplot2
#' @importFrom R6 R6Class


PermuTest <- R6Class(
    classname = "Permutation Test",
    cloneable = FALSE,
    public = list(
        #' @description Create a new `PermuTest` object. Note that it is not recommended to create objects of this class directly. 
        #' 
        #' @param alternative a character string specifying the alternative hypothesis, must be one of `"two_sided"` (default), `"greater"` or `"less"`.
        #' @param n_permu an integer specifying how many permutations should be used to construct the permutation distribution. If `NULL` (default) then all permutations are used.
        #' @param conf_level a number specifying confidence level of the interval.
        #' 
        #' @param scoring a character string specifying which scoring system to be used, must be one of `"none"` (default), `"rank`, `"vw"` or `"savage"`.
        #' 
        #' @return A `PermuTest` object. 
        initialize = function(alternative = c("two_sided", "less", "greater"), n_permu = NULL, conf_level = 0.95, scoring = c("none", "rank", "vw", "savage")) {
            private$.alternative <- match.arg(alternative)
            private$.n_permu <- n_permu

            private$.conf_level <- conf_level

            private$.scoring <- match.arg(scoring)
        },

        #' @description Feed the data to the object. 
        #' 
        #' @param ... the data. 
        #' 
        #' @return The object itself (invisibly).
        feed = function(...) {
            private$.feed(...)
            private$.check()
            private$.calculate()
            invisible(self)
        },

        #' @description Prepare the object for plotting. 
        #' 
        #' @param plot a logical indicating whether to show the histogram.
        #' @param ... extra parameters passed to `ggplot2::stat_bin`.
        #' 
        #' @return A ggplot object containing a histogram of the permutation distribution with a vertical line on it identifying where the statistic lies (invisibly). 
        ggplot = function(plot = TRUE, ...) {
            hist <- ggplot() +
                stat_bin(
                    mapping = aes(x = private$.statistic_permu),
                    geom = "bar", fill = "#68aaa1", ...
                ) +
                geom_vline(
                    xintercept = private$.statistic, linetype = "dashed"
                ) +
                labs(
                    title = "Permutation Distribution", x = "Statistic", y = "Count"
                ) +
                theme(plot.title = element_text(face = "bold", hjust = 0.5))
            if (plot) print(hist)
            invisible(hist)
        }
    ),
    private = list(
        .type = "permu",
        .method = NULL,

        .scoring = NULL,

        .alternative = NULL,

        .n_permu = NULL,

        .data = NULL,
        .data_permu = NULL,
        .statistic = NULL,
        .statistic_permu = NULL,
        .p_value = NULL,

        .estimate = NULL,
        .ci = NULL,
        .conf_level = NULL,

        .statistic_func = NULL,
        .trend = "+",

        # @Override
        .check = function() {},

        # @Override
        .feed = function() {
            # private$.data <- ...
        },

        # @Override
        .permute = function() {
            # private$.data_permu <- ...
        },

        # @Override
        .calculate_scores = function(data) {
            # return(scores)
        },

        # @Override
        .calculate_statistic = function() {
            # private$.statistic <- 
        },

        # @Override
        .calculate_statistic_permu = function() {
            # private$.statistic_permu <- 
        },

        # @Override
        .calculate_p = function() {},

        .calculate_p_permu = function() {
            greater <- mean(private$.statistic_permu >= private$.statistic)
            less <- mean(private$.statistic_permu <= private$.statistic)
            two_sided <- mean(abs(private$.statistic_permu) >= abs(private$.statistic))

            private$.p_value <- switch(
                private$.trend, 
                `+` = switch(private$.alternative,
                    greater = greater, less = less, two_sided = two_sided
                ),
                `-` = switch(private$.alternative,
                    greater = less, less = greater, two_sided = two_sided
                )
            )
        },

        # @Override
        .calculate_estimate = function() {
            # private$.estimate <- 
        },
        # @Override
        .calculate_ci = function() {
            # private$.ci <- 
        },

        .calculate = function() {
            raw_data <- private$.data
            if (private$.scoring != "none") {
                private$.data <- private$.calculate_scores(raw_data)
            }

            private$.calculate_statistic()

            if (private$.type == "permu") {
                private$.permute()
                private$.calculate_statistic_permu()
                private$.calculate_p_permu()
            } else {
                private$.calculate_p()
            }

            private$.calculate_estimate()
            private$.calculate_ci()

            private$.data <- raw_data
        }
    ),
    active = list(
        #' @field type The type. 
        type = function(value) {
            if (missing(value)) {
                private$.type
            } else {
                private$.type <- value
                private$.check()
                private$.calculate()
            }
        },
        #' @field method The method. 
        method = function(value) {
            if (missing(value)) {
                private$.method
            } else {
                private$.method <- value
                private$.check()
                private$.calculate()
            }
        },
        #' @field scoring The scoring system used. 
        scoring = function(value) {
            if (missing(value)) {
                private$.scoring
            } else {
                private$.scoring <- value
                private$.check()
                private$.calculate()
            }
        },
        #' @field alternative The alternative hypothesis. 
        alternative = function(value) {
            if (missing(value)) {
                private$.alternative
            } else {
                private$.alternative <- value
                private$.check()
                private$.calculate()
            }
        },
        #' @field n_permu The number of permutations used. 
        n_permu = function(value) 
            if (missing(value)) {
                private$.n_permu
            } else {
                private$.n_permu <- value
                private$.check()
                if (private$.type == "permu") private$.calculate()
            },
        #' @field conf_level The confidence level of the interval. 
        conf_level = function(value) {
            if (missing(value)) {
                private$.conf_level
            } else {
                private$.conf_level <- value
                private$.check()
                private$.calculate_ci()
            }
        },
        #' @field statistic_func The function used to calculate the statistic. 
        statistic_func = function(value) {
            if (!missing(value)) {
                private$.statistic_func <- value
            }
        },
        #' @field data Data fed into the object. 
        data = function() private$.data,
        #' @field data_permu All permutations used. 
        data_permu = function() private$.data_permu,
        #' @field statistic The test statistic. 
        statistic = function() private$.statistic,
        #' @field statistic_permu Test statistics calculated on permutations. 
        statistic_permu = function() private$.statistic_permu,
        #' @field p_value The p-value. 
        p_value = function() private$.p_value,
        #' @field estimate The estimated parameter. 
        estimate = function() private$.estimate,
        #' @field ci The confidence interval. 
        ci = function() private$.ci
    )
)