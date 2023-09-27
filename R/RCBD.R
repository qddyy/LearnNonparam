#' @title RCBD Class
#' 
#' @description This class specializes `PermuTest` for randomized complete block design. Note that it is not recommended to create objects of this class directly. 
#' 
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom RcppAlgos permuteCount permuteGeneral


RCBD <- R6Class(
    classname = "RCBD",
    inherit = PermuTest,
    cloneable = FALSE,
    private = list(
        .name = "Randomized Complete Block Design",

        .check = function() {},

        .input = function(...) {
            data <- do.call(data.frame, get_data_from(...))

            dim <- dim(data)
            rownames(data) <- paste0("treatment_", seq_len(dim[1]))
            colnames(data) <- paste0("block_", seq_len(dim[2]))

            private$.raw_data <- data
        },

        .calculate_score = function() {
            private$.data <- do.call(
                data.frame, lapply(
                    X = private$.data, FUN = get_score,
                    method = private$.scoring, n = nrow(private$.data)
                )
            )
        },

        .calculate_statistic = function() {
            private$.statistic <- private$.statistic_func(private$.data)
        },

        .calculate_statistic_permu = function() {
            k <- nrow(private$.data)
            b <- ncol(private$.data)

            private$.statistic_permu <- get_arrangement(
                "permute", n_sample = private$.n_permu,
                v = permuteCount(k), m = b, replace = TRUE,
                func = function(index) {
                    statistic_func(do.call(
                        cbind, .mapply(
                            dots = list(data, index),
                            FUN = function(block, i) {
                                permuteGeneral(v = block, lower = i, upper = i)[1, ]
                            }, MoreArgs = NULL
                        )
                    ))
                }, func_value = numeric(1),
                statistic_func = private$.statistic_func,
                data = private$.data
            )
        }
    )
)