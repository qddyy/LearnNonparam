tests <- list(
    onesample.quantile = Quantile,
    onesample.ecdf = ECDF,

    twosample.mean = MeanDiff,
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

    table.chi = ChiSquare
)

tests_df <- data.frame(
    key = names(tests),
    test = unname(vapply(tests, function(t) t$classname, character(1)))
)

#' @title Syntactic Sugar for Object Construction
#' 
#' @description Create a test object conveniently. 
#' 
#' @name pmt


#' @rdname pmt
#' 
#' @param key a character string corresponding to the desired test. Check `pmts()` to see available keys. 
#' @param ... extra parameters passed to the `new()` method of the test class. 
#' 
#' @export
pmt <- function(key, ...) tests[[key]]$new(...)


#' @rdname pmt
#' 
#' @param category a character string specifying which tests to show. 
#' 
#' @export
pmts <- function(category = c("all", "onesample", "twosample", "ksample", "multicomp", "paired", "rcbd", "association", "table")) {
    category <- match.arg(category)

    if (category == "all") tests_df else tests_df[startsWith(tests_df$key, category), ]
}