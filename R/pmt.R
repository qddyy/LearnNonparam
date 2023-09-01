#' @title Syntactic Sugar for Object Construction
#' 
#' @description Create a test object conveniently. 
#' 
#' @name pmt


tests <- list(
    onesample.quantile = Quantile,
    onesample.cdf = CDF,

    twosample.mean = Mean,
    twosample.wilcoxon = Wilcoxon,
    twosample.scoresum = ScoreSum,
    twosample.ansari = AnsariBradley,
    twosample.siegel = SiegelTukey,
    twosample.rmd = RatioMeanDeviance,
    twosample.ks = KolmogorovSmirnov,

    ksample.anova = ANOVA,
    ksample.kw = KruskalWallis,
    ksample.jt = JonckheereTerpstra,

    multicomp.t = MultiCompT,
    multicomp.tukey = TukeyHSD,

    paired.comparison = PairedComparison,
    paired.sign = Sign,
    paired.signedscore = SignedScore,

    rcbd.anova = RCBDANOVA,
    rcbd.friedman = Friedman,
    rcbd.page = Page,

    association.corr = Correlation,

    table.chisq = ChiSquare
)


#' @rdname pmt
#' 
#' @param key a character string corresponding to the desired test. Check `pmts()` to see available keys. 
#' @param ... extra parameters passed to the `new()` method of the test class. 
#' 
#' @export
pmt <- function(key, ...) tests[[key]]$new(...)


#' @rdname pmt
#' 
#' @param which a character string specifying which tests to show. If `"all"` (default) then available tests are shown. 
#' 
#' @export
pmts <- function(which = c("all", "onesample", "twosample", "ksample", "multicomp", "paired", "rcbd", "association", "table")) {
    which <- match.arg(which)

    keys <- names(tests)
    if (which != "all") {
        keys <- keys[startsWith(keys, which)]
    }
    tests <- tests[keys]

    data.frame(
        key = keys,
        class = vapply(
            X = tests, FUN.VALUE = character(1), USE.NAMES = FALSE,
            FUN = function(test) test$classname
        ),
        test = vapply(
            X = tests, FUN.VALUE = character(1), USE.NAMES = FALSE,
            FUN = function(test) test$private_fields$.name
        )
    )
}