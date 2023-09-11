#' @title TwoSamplePairedTest Class
#' 
#' @description This class specializes `TwoSampleTest` for paired two sample permutation tests. Note that it is not recommended to create objects of this class directly. 
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom arrangements permutations


TwoSamplePairedTest <- R6Class(
    classname = "TwoSamplePairedTest",
    inherit = TwoSampleTest,
    cloneable = FALSE,
    private = list(
        .name = "Paired Two Sample Permutation Test",

        .swapped_permu = NULL,
        .use_swapped = TRUE,

        .check = function() {},

        .feed = function(...) {
            super$.feed(...)

            private$.raw_data <- do.call(data.frame, private$.raw_data)
        },

        .calculate_score = function() {},

        .permute = function() {
            private$.swapped_permu <- permutations(
                v = c(TRUE, FALSE), k = nrow(private$.data), replace = TRUE,
                nsample = private$.n_permu, layout = "list"
            )

            private$.data_permu <- lapply(
                X = private$.swapped_permu,
                FUN = function(swapped, x, y) {
                    data.frame(
                        x = `[<-`(x, swapped, y[swapped]),
                        y = `[<-`(y, swapped, x[swapped])
                    )
                }, x = private$.data$x, y = private$.data$y
            )
        },

        .calculate_statistic = function() {
            if (private$.use_swapped) {
                private$.statistic <- private$.statistic_func(
                    swapped = rep.int(FALSE, nrow(private$.data))
                )
            } else {
                super$.calculate_statistic()
            }
        },

        .calculate_statistic_permu = function() {
            if (private$.use_swapped) {
                private$.statistic_permu <- vapply(
                    X = private$.swapped_permu,
                    FUN = private$.statistic_func, FUN.VALUE = numeric(1)
                )
            } else {
                super$.calculate_statistic_permu()
            }
        }
    )
)