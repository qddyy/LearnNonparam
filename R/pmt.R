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
        "ksample",
        "multicomp",
        "paired",
        "rcbd",
        "association",
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