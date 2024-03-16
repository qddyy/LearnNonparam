#' @importFrom Rcpp cppFunction
#' @useDynLib LearnNonparam, .registration = TRUE
NULL

.onLoad <- function(...) {
    if (is.null(getOption("LearnNonparam.pmt_progress"))) {
        options(LearnNonparam.pmt_progress = interactive())
    }
}