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
        "ksample", "multicomp",
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
#' @param inherit a character string specifying the desired permutation test.
#' @param statistic a function defining the test statistic. Can be a closure or a character string defining a `Rcpp` function (excluding its return type and name).
#' @param rejection a character string specifying where the rejection region is.
#' @param scoring,n_permu passed to the constructor.
#' @param name a character string specifying the name of the test.
#' @param alternative a character string specifying the alternative of the test.
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' @importFrom Rcpp sourceCpp
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

    if (!missing(scoring) && inherit %in% c("paired", "association", "table")) {
        warning("Ignoring 'scoring' since 'type' is set to '", inherit, "'")
        scoring <- "none"
    }

    if (is.character(statistic)) {
        xptr_code <- paste(
            "#include <Rcpp.h>",
            "using namespace Rcpp;",
            "// [[Rcpp::depends(LearnNonparam)]]",
            "#include <alias.hpp>",
            paste0(inherit, "_closure", " user_defined", statistic),
            "// [[Rcpp::export]]",
            {
                T <- paste0(inherit, "_func")
                paste0(
                    "XPtr<", T, "> get_xptr()",
                    "{ return XPtr<", T, ">(new ", T, "(&user_defined)); }"
                )
            },
            sep = "\n"
        )
        xptr_env <- new.env()
        sourceCpp(code = xptr_code, env = xptr_env, embeddedR = FALSE)
        statistic <- xptr_env$get_xptr()
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

    multicomp.studentized = Studentized,

    paired.sign = Sign,
    paired.difference = PairedDifference,

    rcbd.f = RCBDF,
    rcbd.friedman = Friedman,
    rcbd.page = Page,

    association.corr = Correlation,

    table.chisq = ChiSquare
)