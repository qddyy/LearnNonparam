#' @title Syntactic Sugar for Object Construction
#' 
#' @description Construct test objects in a unified way.
#' 
#' @name pmt

#' @rdname pmt
#' 
#' @param key a character string corresponding to the desired test. Check `pmts()` for valid keys.
#' @param ... extra parameters passed to the constructor.
#' 
#' @export

pmt <- function(key, ...) {
    if (key %in% names(implemented)) {
        implemented[[key]]$new(...)
    } else {
        stop(
            "The key '", key,
            "' is not valid. ",
            "Check `pmts()` for valid keys."
        )
    }
}


#' @rdname pmt
#' 
#' @param which a character string specifying the desired tests.
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

    ksample.f = KSampleF,
    ksample.kw = KruskalWallis,
    ksample.jt = JonckheereTerpstra,

    multcomp.studentized = Studentized,

    paired.sign = Sign,
    paired.difference = PairedDifference,

    rcbd.f = RCBDF,
    rcbd.friedman = Friedman,
    rcbd.page = Page,

    association.corr = Correlation,

    table.chisq = ChiSquare
)

#' @rdname pmt
#' 
#' @param statistic definition of the test statistic. See `Details`.
#' @param depends,plugins,includes passed to `Rcpp::cppFunction`.
#' @param inherit a character string specifying the desired permutation test.
#' @param rejection a character string specifying where the rejection region is.
#' @param scoring,n_permu passed to the constructor.
#' @param name a character string specifying the name of the test.
#' @param alternative a character string specifying the alternative of the test.
#' 
#' @details The test statistic in `define_pmt` can be defined using either `R` or `Rcpp`, with the `statistic` parameter specified as:
#' 
#' - `R`: a closure returning another closure.
#' - `Rcpp`: a character string defining a captureless lambda (introduced in C++11) returning another lambda that may capture by value, accepts const arguments of the same type, and returns a double.
#' 
#' When using `Rcpp`, the parameters for different `inherit` are listed as follows. Note that the parameter names are illustrative and may be modified.
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
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom Rcpp cppFunction
#' @importFrom compiler cmpfun

define_pmt <- function(
    statistic,
    depends = character(), plugins = character(), includes = character(),
    inherit = c(
        "twosample", "ksample",
        "paired", "rcbd",
        "association", "table"
    ),
    rejection = c("lr", "l", "r"),
    scoring = c("none", "rank", "vw", "expon"), n_permu = 1e4,
    name = "User-Defined Permutation Test", alternative = NULL
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
            initialize = function(n_permu) {
                self$n_permu <- n_permu

                if (typeof(statistic) == "closure") {
                    private$.statistic_func <- cmpfun(statistic)
                } else if (!is.character(statistic)) {
                    stop("'statistic' must be a closure or a character string")
                } else {
                    user_pmt <- cppFunction(
                        depends = c(depends, "LearnNonparam"),
                        plugins = unique(c(plugins, "cpp14")),
                        includes = c(
                            includes,
                            "#include <pmt/pmt_alias.hpp>",
                            "#include <pmt/pmt_macros.hpp>",
                            "#include <pmt/pmt_progress.hpp>",
                            "#include <pmt/pmt_reorder.hpp>",
                            paste0("#include <pmt/impl_", inherit, "_pmt.hpp>")
                        ),
                        code = {
                            n_x <- if (inherit %in% c("rcbd", "table")) 2 else 3
                            paste0(
                                "SEXP user_pmt(",
                                paste0("SEXP x", seq_len(n_x), collapse = ","),
                                ",R_xlen_t n_permu,bool progress){",
                                "auto statistic_func=", statistic, ";",
                                "PMT_PROGRESS_RETURN(impl_", inherit, "_pmt,",
                                inherit, "_func,", inherit, "_closure,",
                                paste0("x", seq_len(n_x - 1), collapse = ","), ")}"
                            )
                        }
                    )
                    assign(
                        paste0(inherit, "_pmt"), user_pmt,
                        envir = environment(super$.calculate_statistic_permu)
                    )
                }
            }
        ),
        private = list(
            .name = if (!missing(name)) as.character(name) else name,
            .alternative = if (!missing(alternative)) as.character(alternative),
            .scoring = match.arg(scoring),
            .side = match.arg(rejection),

            .calculate = function() {
                private$.preprocess()

                if (private$.scoring != "none") {
                    private$.calculate_score()
                }

                private$.calculate_statistic_permu()
                private$.calculate_n_permu()
                private$.calculate_p_permu()
            },

            .print = function(...) {
                super$.print(...)

                if (is.null(private$.alternative)) {
                    cat("\33[1A                       ")
                }
            }
        )
    )$new(n_permu)
}
