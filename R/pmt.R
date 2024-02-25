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
#' @param statistic a function defining the test statistic.
#' @param side a character string specifying where the rejection region is.
#' @param scoring a character string specifying which scoring system to be used.
#' @param n_permu an integer indicating how many permutations should be used to construct the permutation distribution.
#' @param name a character string specifying the name of the test.
#' @param alternative a character string specifying the alternative of the test.
#' 
#' @export
#' 
#' @importFrom R6 R6Class
define_pmt <- function(
    inherit = c(
        "twosample",
        "ksample", "multicomp",
        "paired", "rcbd",
        "association", "table"
    ),
    statistic = NULL, side = c("lr", "l", "r"),
    scoring = c("none", "rank", "vw", "expon"), n_permu = 0L,
    name = "User-Defined Permutation Test", alternative = "not specified"
) {
    inherit <- match.arg(inherit)

    class <- switch(inherit,
        twosample = TwoSampleTest,
        ksample = KSampleTest,
        multicomp = MultipleComparison,
        paired = TwoSamplePairedTest,
        rcbd = RCBDTest,
        association = TwoSampleAssociationTest,
        table = ContingencyTableTest
    )

    R6Class(
        classname = "UserDefined",
        inherit = class,
        cloneable = FALSE,
        public = list(
            initialize = function(n_permu) {
                self$n_permu <- n_permu
                private$.statistic_func <- statistic
            }
        ),
        private = list(
            .name = as.character(name),
            .alternative = as.character(alternative),
            .scoring = match.arg(scoring),
            .side = match.arg(side),

            .calculate_side = function() NULL
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