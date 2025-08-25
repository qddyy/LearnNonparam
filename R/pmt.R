implemented <- list(
    onesample.quantile = Quantile,
    onesample.cdf = CDF,

    twosample.difference = Difference,
    twosample.wilcoxon = Wilcoxon,
    twosample.scoresum = ScoreSum,
    twosample.ansari = AnsariBradley,
    twosample.siegel = SiegelTukey,
    twosample.rmd = RatioMeanDeviance,

    distribution.ks = KolmogorovSmirnov,
    distribution.kuiper = Kuiper,
    distribution.cvm = CramerVonMises,
    distribution.ad = AndersonDarling,

    association.corr = Correlation,

    paired.sign = Sign,
    paired.difference = PairedDifference,

    ksample.oneway = OneWay,
    ksample.kw = KruskalWallis,
    ksample.jt = JonckheereTerpstra,

    multcomp.studentized = Studentized,

    rcbd.oneway = RCBDOneWay,
    rcbd.friedman = Friedman,
    rcbd.page = Page,

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
        "twosample", "distribution", "association",
        "paired",
        "ksample", "multcomp",
        "rcbd",
        "table"
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
#' @param method a character string specifying the permutation scheme.
#' @param statistic definition of the test statistic. See details.
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
#' The test statistic can be defined using either R or Rcpp, with the `statistic` parameter specified as:
#' 
#' - R: a function returning a closure that returns a double.
#' - Rcpp: a character string defining a captureless lambda (since C++11) returning another lambda that captures by value, accepts parameters of the same type, and returns a double.
#' 
#' The purpose of this design is to pre-calculate certain constants that remain invariant during permutation.
#' 
#' When using Rcpp, the parameters for different `method` are listed as follows. Note that the names can be customized, and the types can be replaced with `auto` (thanks to the support for generic lambdas in C++14). See examples.
#' 
#' | `method`         | Parameter 1                                 | Parameter 2                                  |
#' |:----------------:|:-------------------------------------------:|:--------------------------------------------:|
#' | `"twosample"`    | `const NumericVector& sample_1`             | `const NumericVector& sample_2`              |
#' | `"distribution"` | `const NumericVector& cumulative_prob_1`    | `const NumericVector& cumulative_prob_2`     |
#' | `"association"`  | `const NumericVector& sample_1`             | `const NumericVector& sample_2`              |
#' | `"paired"`       | `const NumericVector& sample_1`             | `const NumericVector& sample_2`              |
#' | `"ksample"`      | `const NumericVector& combined_sample`      | `const IntegerVector& one_based_group_index` |
#' | `"rcbd"`         | `const NumericMatrix& block_as_column_data` |                                              |
#' | `"table"`        | `const IntegerMatrix& contingency_table`    |                                              |
#' 
#' When using R, the parameters should be the R equivalents of these.
#' 
#' @note
#' - `statistic` should not cause errors or return missing values.
#' - The data is permuted in-place. Therefore, modifications to the data within `statistic` may lead to incorrect results. Since R has copy-on-modify semantics but C++ does not, it is recommended to pass const references when using Rcpp in `define_pmt`, as shown in the table above.
#' 
#' @examples
#' x <- rnorm(5)
#' y <- rnorm(5, 1)
#' 
#' t <- define_pmt(
#'     method = "twosample",
#'     scoring = base::rank, # equivalent to "rank"
#'     statistic = function(...) function(x, y) sum(x)
#' )$test(x, y)$print()
#' 
#' t$scoring <- function(x) qnorm(rank(x) / (length(x) + 1)) # equivalent to "vw"
#' t$print()
#' 
#' t$n_permu <- 0
#' t$print()
#' 
#' \donttest{
#' r <- define_pmt(
#'     method = "twosample", n_permu = 1e5,
#'     statistic = function(x, y) {
#'         m <- length(x)
#'         n <- length(y)
#'         function(x, y) sum(x) / m - sum(y) / n
#'     }
#' )
#' 
#' 
#' rcpp <- define_pmt(
#'     method = "twosample", n_permu = 1e5,
#'     statistic = "[](const auto& x, const auto& y) {
#'         auto m = x.length();
#'         auto n = y.length();
#'         return [=](const auto& x, const auto& y) {
#'             return sum(x) / m - sum(y) / n;
#'         };
#'     }"
#' )
#' 
#' # equivalent
#' # rcpp <- define_pmt(
#' #     method = "twosample", n_permu = 1e5,
#' #     statistic = "[](const NumericVector& x, const NumericVector& y) {
#' #         R_xlen_t m = x.length();
#' #         R_xlen_t n = y.length();
#' #         return [m, n](const NumericVector& x, const NumericVector& y) -> double {
#' #             return sum(x) / m - sum(y) / n;
#' #         };
#' #     }"
#' # )
#' 
#' options(LearnNonparam.pmt_progress = FALSE)
#' system.time(r$test(x, y))
#' system.time(rcpp$test(x, y))
#' }
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom Rcpp evalCpp cppFunction

define_pmt <- function(
    method = c(
        "twosample", "distribution", "association",
        "paired",
        "ksample",
        "rcbd",
        "table"
    ),
    statistic,
    rejection = c("lr", "l", "r"),
    scoring = "none", n_permu = 1e4,
    name = "User-Defined Permutation Test", alternative = NULL,
    depends = character(), plugins = character(), includes = character()
) {
    method <- match.arg(method)

    if (!missing(scoring) && method %in% c("distribution", "paired", "table")) {
        warning("Ignoring 'scoring' since 'method' is set to '", method, "'")
        scoring <- "none"
    }

    self <- super <- private <- NULL
    R6Class(
        classname = "UserDefined",
        cloneable = FALSE,
        inherit = switch(method,
            twosample = TwoSampleTest,
            distribution = TwoSampleDistributionTest,
            association = TwoSampleAssociationTest,
            paired = TwoSamplePairedTest,
            ksample = KSampleTest,
            rcbd = RCBDTest,
            table = ContingencyTableTest
        ),
        public = list(
            initialize = function(scoring, n_permu) {
                self$scoring <- scoring
                self$n_permu <- n_permu

                if (typeof(statistic) == "closure") {
                    private$.statistic_func <- statistic
                    private$.compile()
                } else if (!is.character(statistic) || length(statistic) > 1) {
                    stop("'statistic' must be a closure or a character string")
                } else {
                    method <- if (method == "distribution") "table" else method
                    impl <- paste0("impl_", method, "_pmt")
                    cppFunction(
                        depends = c(depends, "LearnNonparam"),
                        plugins = {
                            cpp_standard_ver <- evalCpp("__cplusplus")
                            c(plugins, if (cpp_standard_ver < 201402L) "cpp14")
                        },
                        includes = {
                            hpps <- c("permutation", "progress", impl)
                            c(includes, paste0("#include<pmt/", hpps, ".hpp>"))
                        },
                        env = environment(super$.calculate_statistic),
                        code = {
                            n <- if (method %in% c("rcbd", "table")) 2L else 3L
                            args <- paste0("arg_", seq_len(n))
                            paste0(
                                "SEXP ", method, "_pmt(",
                                paste("SEXP", args, collapse = ","),
                                ", double n_permu, bool progress){",
                                "auto statistic = ", statistic, ";",
                                "return progress ?", paste0(
                                    impl, "<", c("true", "false"), ">(", paste(
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
            .method = method,

            .side = match.arg(rejection),

            .name = if (!missing(name)) as.character(name) else name,
            .alternative = if (!missing(alternative)) as.character(alternative),

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