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
#' @param inherit a character string specifying the desired permutation test.
#' @param statistic definition of the test statistic. See `Details`.
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
    inherit = c(
        "twosample", "ksample",
        "paired", "rcbd",
        "association", "table"
    ),
    statistic = NULL, rejection = c("lr", "l", "r"),
    scoring = c("none", "rank", "vw", "expon"), n_permu = 0L,
    name = "User-Defined Permutation Test", alternative = NULL
) {
    inherit <- match.arg(inherit)

    if (!missing(scoring) && inherit %in% c("paired", "table")) {
        warning("Ignoring 'scoring' since 'inherit' is set to '", inherit, "'")
        scoring <- "none"
    }

    if (is.character(statistic)) {
        statistic_xptr <- cppFunction(
            depends = "LearnNonparam",
            includes = "#include <alias.hpp>",
            code = {
                T <- paste0(inherit, "_func")
                paste0(
                    "XPtr<", T, "> statistic_xptr() {",
                    T, " user_defined(", statistic, ");",
                    "return XPtr<", T, ">(new ", T, "(user_defined)); }"
                )
            }
        )
        statistic <- statistic_xptr()
    } else if (typeof(statistic) == "closure") {
        statistic <- cmpfun(statistic)
    } else {
        stop("'statistic' must be a closure or a character string")
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
                private$.statistic_func <- statistic
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
    )$new(n_permu = n_permu)
}