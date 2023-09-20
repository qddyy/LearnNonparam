#' @importFrom R6 R6Class


ProgressBar <- R6Class(
    classname = "ProgressBar",
    cloneable = FALSE,
    private = list(
        .n_steps = NULL,

        .percentage = 0,

        .update_i = 0,
        .update_every = NULL,
        .update_percentage = NULL,

        .width = NULL
    ),
    public = list(
        initialize = function(n) {
            private$.n_steps <- n
            private$.update_i <- 0
            private$.update_every <- round(private$.n_steps / 100 + 0.5)
            private$.update_percentage <- private$.update_every / private$.n_steps
            private$.width <- getOption("width")
        },

        update = function() {
            private$.update_i <- private$.update_i + 1

            if (private$.update_i %% private$.update_every == 0) {
                private$.update_i <- 0
                percentage <- private$.percentage <- (
                    private$.percentage + private$.update_percentage
                )

                cat(
                    "\033[0;31m", sprintf("\r %.0f%% |", percentage * 100),
                    strrep("=", private$.width * percentage), ">\r", sep = ""
                )
                flush.console()
            }
        },

        close = function() {
            cat("\033[0m\r", strrep(" ", private$.width))
            flush.console()
        }
    )
)