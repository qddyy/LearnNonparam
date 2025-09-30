implemented <- list(
    onesample.quantile = Quantile,
    onesample.cdf = CDF,

    twosample.difference = Difference,
    twosample.wilcoxon = Wilcoxon,
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
#' @param rejection a character string specifying the rejection region relative to the test statistic.
#' @param scoring one of:
#'      - a character string in `c("none", "rank", "vw", "expon")` specifying the scoring system
#'      - a function that takes a numeric vector and returns an equal-length score vector
#' @param n_permu an integer indicating number of permutations for the permutation distribution. If set to `0`, all permutations will be used.
#' @param name,alternative character strings specifying the name of the test and the alternative hypothesis, used for printing purposes only.
#' @param quickr a logical indicating whether to use [quickr::quick()] to accelerate `statistic`. See details.
#' @param depends,plugins,includes passed to [Rcpp::cppFunction()].
#' 
#' @return a test object based on the specified statistic.
#' 
#' @details
#' The test statistic can be defined using either R or Rcpp, with the `statistic` parameter specified as:
#' 
#' - **R**: a closure returning one of
#'   - a double (the test statistic).
#'   - a closure returning a double.
#' - **Rcpp**: a character string defining a captureless lambda (since C++11) returning another lambda that captures by value, accepts parameters of the same type, and returns a double.
#' 
#' This design aims to pre-calculate potential constants that remain invariant during permutation.
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
#' When using R, `statistic` and the parameters should be the R equivalents of the above. If no constants exist during permutation, `statistic` may simply be an R closure returning a double.
#' 
#' If `quickr = TRUE` and `statistic` returns a double, it will be compiled to Fortran via [quickr::quick()] with [base::declare()] calls for all arguments inserted automatically. Otherwise, `statistic` will be compiled using [compiler::cmpfun()].
#' 
#' @note
#' To improve performance when calling R closures from C++, this package repeatedly evaluates the closure's body in an environment whose enclosing environment is the closure’s own, with its formal arguments pre-assigned to the data. This imposes the following restrictions on the closure returning the test statistic when `statistic` is written in R:
#' - Do not re-assign its formal arguments or any pre-computed symbols in its environment.
#' - Do not use default arguments or variadic arguments.
#' 
#' It's also worth noting that the data is permuted in-place. Therefore, modifications to the data within `statistic` may lead to incorrect results. It is recommended to avoid modifying the data when using R and pass const references as in the table above when using Rcpp.
#' 
#' @examples
#' x <- rnorm(5)
#' y <- rnorm(5, 1)
#' 
#' t <- define_pmt(
#'     method = "twosample", rejection = "<",
#'     scoring = base::rank, # equivalent to "rank"
#'     statistic = function(x, y) sum(x)
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
#' quickr <- define_pmt(
#'     method = "twosample", n_permu = 1e5, quickr = TRUE,
#'     statistic = function(x, y) sum(x) / length(x) - sum(y) / length(y)
#' )
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
#' set.seed(0)
#' r$test(x, y)$print()
#' set.seed(0)
#' quickr$test(x, y)$print()
#' set.seed(0)
#' rcpp$test(x, y)$print()
#' 
#' options(LearnNonparam.pmt_progress = FALSE)
#' system.time(r$test(x, y))
#' system.time(quickr$test(x, y))
#' system.time(rcpp$test(x, y))
#' }
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom Rcpp evalCpp cppFunction
#' @importFrom compiler cmpfun

define_pmt <- function(
    method = c(
        "twosample", "distribution", "association",
        "paired",
        "ksample",
        "rcbd",
        "table"
    ),
    statistic,
    rejection = c("<>", "<", ">"),
    scoring = "none", n_permu = 1e4,
    name = "User-Defined Permutation Test", alternative = NULL,
    quickr = FALSE,
    depends = character(),
    plugins = character(),
    includes = character()
) {
    method <- match.arg(method)

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

                n <- if (method %in% c("rcbd", "table")) 1 else 2

                if (typeof(statistic) == "closure") {
                    compiled <- NULL
                    private$.statistic_func <- function(...) {
                        o <- closure <- statistic(...)

                        if (typeof(closure) == "closure") {
                            closure <- cmpfun(closure)
                            o <- closure(...)
                        } else {
                            if (!is.null(compiled)) {
                                return(compiled)
                            }

                            closure <- statistic
                            if (!quickr) {
                                closure <- cmpfun(closure)
                            } else if (requireNamespace("quickr")) {
                                body(closure) <- call(
                                    "{",
                                    as.call(c(
                                        as.symbol("declare"),
                                        .mapply(
                                            dots = list(
                                                names(formals(closure)),
                                                switch(method,
                                                    twosample = expression(
                                                        double(NA), double(NA)
                                                    ),
                                                    distribution = expression(
                                                        double(n), double(n)
                                                    ),
                                                    association = expression(
                                                        double(n), double(n)
                                                    ),
                                                    paired = expression(
                                                        double(n), double(n)
                                                    ),
                                                    ksample = expression(
                                                        double(n), integer(n)
                                                    ),
                                                    rcbd = expression(
                                                        double(NA, NA)
                                                    ),
                                                    table = expression(
                                                        integer(NA, NA)
                                                    )
                                                )
                                            ),
                                            FUN = function(arg, type) {
                                                as.call(c(
                                                    as.symbol("type"),
                                                    `names<-`(list(type), arg)
                                                ))
                                            },
                                            MoreArgs = NULL
                                        )
                                    )),
                                    body(closure)
                                )
                                closure <- eval(
                                    bquote(
                                        quickr::quick(.(closure))
                                    ),
                                    envir = environment(closure)
                                )
                            }

                            compiled <<- closure
                        }

                        if (length(formals(closure)) != n) {
                            compiled <<- NULL
                            stop(
                                "Expected ", n, " formal arguments",
                                " when 'method' is '", method, "'"
                            )
                        }
                        if (!is.numeric(o) || length(o) != 1) {
                            compiled <<- NULL
                            stop(
                                "'statistic' must return a double",
                                " or a closure returning a double"
                            )
                        }

                        closure
                    }
                } else if (!is.character(statistic) || length(statistic) > 1) {
                    stop("'statistic' must be a closure or a character string")
                } else {
                    cppFunction(
                        depends = c(depends, "LearnNonparam"),
                        plugins = {
                            cpp_standard_ver <- evalCpp("__cplusplus")
                            c(plugins, if (cpp_standard_ver < 201402L) "cpp14")
                        },
                        includes = local({
                            if (method == "distribution") {
                                method <- "table"
                            }
                            impl <- paste0("impl_", method, "_pmt")
                            hpps <- c("permutation", "progress", impl)
                            c(includes, paste0("#include<pmt/", hpps, ".hpp>"))
                        }),
                        env = environment(super$.calculate_statistic),
                        code = {
                            args <- paste0("arg", seq_len(n <- n + 1))
                            paste0(
                                "SEXP ", method, "_pmt(",
                                paste("SEXP", args, collapse = ","),
                                ", double n_permu, bool progress){",
                                "auto statistic = ", statistic, ";",
                                "return progress ? ", paste0(
                                    "impl_", method, "_pmt<",
                                    c("true", "false"), ">(", paste(
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

            .side = switch(match.arg(rejection),
                `<>` = "lr", `<` = "l", `>` = "r"
            ),

            .name = if (!missing(name)) as.character(name) else name,
            .alternative = if (!missing(alternative)) as.character(alternative),

            .calculate = function() {
                private$.preprocess()

                if (private$.scoring != "none") {
                    private$.calculate_score()
                }

                private$.calculate_statistic()
                if (
                    is.na(private$.statistic) ||
                    anyNA(attr(private$.statistic, "permu"))
                ) {
                    warning("NAs produced")
                }

                private$.calculate_n_permu()
                private$.calculate_p_permu()
            }
        ),
        active = list(
            scoring = function(value) {
                if (missing(value)) {
                    if (private$.scoring == "custom") {
                        return(environment(super$.calculate_score)$get_score)
                    } else {
                        return(private$.scoring)
                    }
                }

                if (is.character(value)) {
                    private$.scoring <- match.arg(
                        value, choices = c("none", "rank", "vw", "expon")
                    )
                    if (private$.scoring != "none" && private$.method %in% c(
                        "distribution", "paired", "table"
                    )) {
                        warning(
                            "'scoring' forced to 'none' since 'method' is ",
                            "'", private$.method, "'"
                        )
                        private$.scoring <- "none"
                    }
                } else if (typeof(value) == "closure") {
                    get_score_ <- function(x, ...) {
                        score <- value(x)
                        if (!is.numeric(score) || length(score) != length(x)) {
                            stop("Invalid scoring system")
                        } else score
                    }
                    environment(super$.calculate_score)$get_score <- get_score_
                    private$.scoring <- "custom"
                } else {
                    stop("'scoring' must be a character string or a closure")
                }

                if (!is.null(private$.raw_data)) {
                    private$.on_scoring_change()
                }
            }
        )
    )$new(scoring, n_permu)
}