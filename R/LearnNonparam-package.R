#' @useDynLib LearnNonparam, .registration = TRUE
NULL

.onLoad <- function(...) {
    if (is.null(getOption("LearnNonparam.pmt_progress"))) {
        options(LearnNonparam.pmt_progress = interactive())
    }

    if (requireNamespace("ggplot2", quietly = TRUE)) {
        register <- function(...) {
            registerS3method(
                genname = "autoplot", class = "PermuTest",
                method = autoplot.PermuTest, envir = getNamespace("ggplot2")
            )
        }
        setHook(packageEvent("ggplot2", "onLoad"), register)
        if (isNamespaceLoaded("ggplot2")) {
            register()
        }
    }
}