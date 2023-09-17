#' @importFrom R6 R6Class


ProgressBar <- R6Class(
    classname = "ProgressBar",
    cloneable = FALSE,
    private = list(
        .step = 0,
        .n_steps = NULL,
        .update_every = NULL,
        .width = NULL
    ),
    public = list(
        initialize = function(n) {
            private$.n_steps <- n
            private$.update_every <- ceiling(private$.n_steps / 100)
            private$.width <- getOption("width")
        },

        update = function() {
            private$.step <- private$.step + 1

            if (private$.step %% private$.update_every == 0) {
                percentage <- private$.step / private$.n_steps
                cat(
                    "\033[0;31m",
                    sprintf("\r %.0f%% >", percentage * 100),
                    strrep("=", private$.width * percentage)
                )
                flush.console()
            }
        },

        close = function() {
            cat("\r", strrep(" ", private$.width))
            flush.console()
        }
    )
)