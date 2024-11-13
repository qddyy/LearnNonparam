implemented <- list(
    onesample.quantile = Quantile,
    onesample.cdf = CDF,

    twosample.difference = Difference,
    twosample.wilcoxon = Wilcoxon,
    twosample.scoresum = ScoreSum,
    twosample.ansari = AnsariBradley,
    twosample.siegel = SiegelTukey,
    twosample.rmd = RatioMeanDeviance,
    twosample.ks = KolmogorovSmirnov,

    ksample.oneway = OneWay,
    ksample.kw = KruskalWallis,
    ksample.jt = JonckheereTerpstra,

    multcomp.studentized = Studentized,

    paired.sign = Sign,
    paired.difference = PairedDifference,

    rcbd.oneway = RCBDOneWay,
    rcbd.friedman = Friedman,
    rcbd.page = Page,

    association.corr = Correlation,

    table.chisq = ChiSquare
)


#' @title Syntactic Sugar for Object Construction
#' 
#' @description Construct test objects in a unified way.
#' 
#' @name pmt


#' @rdname pmt
#' 
#' @param key a character string specifying the test. Check `pmts()` for valid keys.
#' @param ... extra parameters passed to the constructor.
#' 
#' @return a test object corresponding to the specified key.
#' 
#' @examples pmt("twosample.wilcoxon")
#' 
#' @export

pmt <- function(key, ...) {
    implemented[[match.arg(key, choices = names(implemented))]]$new(...)
}


#' @rdname pmt
#' 
#' @param which a character string specifying the desired tests.
#' 
#' @return a data frame containing keys and corresponding tests implemented in this package.
#' 
#' @examples pmts("ksample")
#' 
#' @export

pmts <- function(
    which = c(
        "all",
        "onesample",
        "twosample",
        "ksample", "multcomp",
        "paired", "rcbd",
        "association", "table"
    )
) {
    which <- match.arg(which)

    keys <- names(implemented)
    if (which != "all") {
        keys <- keys[startsWith(keys, which)]
    }

    data.frame(
        key = keys,
        class = vapply(
            X = implemented[keys], FUN.VALUE = character(1), USE.NAMES = FALSE,
            FUN = function(test) test$classname
        ),
        test = vapply(
            X = implemented[keys], FUN.VALUE = character(1), USE.NAMES = FALSE,
            FUN = function(test) test$private_fields$.name
        )
    )
}


#' @rdname pmt
#' 
#' @param inherit a character string specifying the type of permutation test.
#' @param statistic definition of the test statistic. See Details.
#' @param rejection a character string specifying where the rejection region is.
#' @param scoring one of:
#'      - a character string in `c("none", "rank", "vw", "expon")` specifying the scoring system
#'      - a function that takes a numeric vector and returns an equal-length score vector
#' @param n_permu an integer indicating number of permutations for the permutation distribution. If set to `0`, all permutations will be used.
#' @param name a character string specifying the name of the test.
#' @param alternative a character string describing the alternative hypothesis.
#' @param depends,plugins,includes passed to [Rcpp::cppFunction()].
#' 
#' @return a test object based on the specified statistic.
#' 
#' @details
#' The test statistic can be defined using either `R` or `Rcpp`, with the `statistic` parameter specified as:
#' 
#' - `R`: a function returning a closure that returns a double.
#' - `Rcpp`: a character string defining a captureless lambda (since C++11) returning another lambda that captures by value, accepts parameters of the same type as const references, and returns a double.
#' 
#' When using `Rcpp`, the parameters for different `inherit` are listed as follows. Note that the parameter names are for illustration only.
#' 
#' - `"twosample"`: `(Rcpp::NumericVector sample_1, Rcpp::NumericVector sample_2)`
#' - `"ksample"`: `(Rcpp::NumericVector combined_sample, Rcpp::IntegerVector one_based_group_index)`
#' - `"paired"`: `(Rcpp::NumericVector sample_1, Rcpp::NumericVector sample_2)`
#' - `"rcbd"`: `(Rcpp::NumericMatrix block_as_column_data)`
#' - `"association"`: `(Rcpp::NumericVector sample_1, Rcpp::NumericVector sample_2)`
#' - `"table"`: `(Rcpp::IntegerMatrix contingency_table)`
#' 
#' Defining the test statistic using `R` follows a similar approach. The purpose of this design is to pre-calculate certain constants that remain invariant during permutation.
#' 
#' @examples
#' x <- rnorm(100)
#' y <- rnorm(100, 1)
#' 
#' t <- define_pmt(
#'     inherit = "twosample",
#'     scoring = base::rank, # equivalent to "rank"
#'     statistic = function(...) function(x, y) sum(x)
#' )$test(x, y)$print()
#' 
#' \donttest{
#' r <- define_pmt(
#'     inherit = "twosample",
#'     n_permu = 1e5,
#'     statistic = function(x, y) {
#'         m <- length(x)
#'         n <- length(y)
#'         function(x, y) sum(x) / m - sum(y) / n
#'     }
#' )
#' 
#' rcpp <- define_pmt(
#'     inherit = "twosample",
#'     n_permu = 1e5,
#'     statistic = "[](NumericVector x, NumericVector y) {
#'         R_len_t m = x.size();
#'         R_len_t n = y.size();
#'         return [=](const NumericVector& x, const NumericVector& y) -> double {
#'             return sum(x) / m - sum(y) / n;
#'         };
#'     }"
#' )
#' 
#' options(LearnNonparam.pmt_progress = FALSE)
#' system.time(r$test(x, y))
#' system.time(rcpp$test(x, y))
#' }
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom Rcpp cppFunction evalCpp
#' @importFrom compiler cmpfun

define_pmt <- function(
    inherit = c(
        "twosample", "ksample", "paired", "rcbd", "association", "table"
    ),
    statistic,
    rejection = c("lr", "l", "r"),
    scoring = "none", n_permu = 1e4,
    name = "User-Defined Permutation Test", alternative = NULL,
    depends = character(), plugins = character(), includes = character()
) {
    inherit <- match.arg(inherit)

    if (!missing(scoring) && inherit %in% c("paired", "table")) {
        warning("Ignoring 'scoring' since 'inherit' is set to '", inherit, "'")
        scoring <- "none"
    }

    self <- super <- private <- NULL
    R6Class(
        classname = "UserDefined",
        cloneable = FALSE,
        inherit = switch(inherit,
            twosample = TwoSampleTest,
            ksample = KSampleTest,
            paired = TwoSamplePairedTest,
            rcbd = RCBDTest,
            association = TwoSampleAssociationTest,
            table = ContingencyTableTest
        ),
        public = list(
            initialize = function(scoring, n_permu) {
                self$scoring <- scoring
                self$n_permu <- n_permu

                if (typeof(statistic) == "closure") {
                    private$.statistic_func <- cmpfun(statistic)
                } else if (!is.character(statistic)) {
                    stop("'statistic' must be a closure or a character string")
                } else {
                    impl <- paste0("impl_", inherit, "_pmt")
                    cppFunction(
                        env = environment(super$.calculate_statistic),
                        depends = c(depends, "LearnNonparam"),
                        plugins = {
                            cpp_standard_ver <- evalCpp("__cplusplus")
                            c(plugins, if (cpp_standard_ver < 201402L) "cpp14")
                        },
                        includes = {
                            hpps <- c("progress", "reorder", impl)
                            c(includes, paste0("#include<pmt/", hpps, ".hpp>"))
                        },
                        code = {
                            args <- paste0(
                                "arg", 1:(n <- if (inherit == "rcbd") 2 else 3)
                            )
                            paste0(
                                "SEXP ", inherit, "_pmt(",
                                paste("SEXP", args, collapse = ","),
                                ", double n_permu, bool progress){",
                                "auto statistic = ", statistic, ";",
                                "return progress ?", paste0(
                                    impl, "<PermuBar", c("Show", "Hide"), ">(",
                                    paste0(
                                        "clone(", args[-n], ")", collapse = ","
                                    ), ", statistic, n_permu )", collapse = ":"
                                ), ";}"
                            )
                        }
                    )
                }
            }
        ),
        private = list(
            .name = if (!missing(name)) as.character(name) else name,
            .alternative = if (!missing(alternative)) as.character(alternative),

            .side = match.arg(rejection),

            .calculate = function() {
                private$.preprocess()

                if (private$.scoring != "none") {
                    private$.calculate_score()
                }

                private$.calculate_statistic()
                private$.calculate_n_permu()
                private$.calculate_p_permu()
            }
        ),
        active = list(
            scoring = function(value) {
                if (missing(value)) {
                    return(private$.scoring)
                } else if (is.character(value)) {
                    private$.scoring <- match.arg(
                        value, choices = c("none", "rank", "vw", "expon")
                    )
                } else if (is.function(value)) {
                    private$.scoring <- "custom"
                    get_score <- function(x, ...) {
                        score <- value(x)
                        if (!is.numeric(score) || length(score) != length(x)) {
                            stop("Invalid scoring system")
                        } else score
                    }
                } else {
                    stop("'scoring' must be a character string or a function")
                }

                environment(super$.calculate_score)$get_score <- get_score

                if (!is.null(private$.raw_data)) {
                    private$.on_scoring_change()
                }
            }
        )
    )$new(scoring, n_permu)
}