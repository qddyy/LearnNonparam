tests <- as.environment(list(
    onesample.quantile = Quantile$new,
    onesample.ecdf = ECDF$new,

    twosample.mean = MeanDiff$new,
    twosample.wilcoxon = Wilcoxon$new,
    twosample.scoresum = ScoreSum$new,
    twosample.ansari = AnsariBradley$new,
    twosample.siegel = SiegelTukey$new,
    twosample.rmd = RatioMeanDeviance$new,
    twosample.ks = KolmogorovSmirnov$new,

    ksample.anova = ANOVA$new,
    ksample.kw = KruskalWallis$new,
    ksample.jt = JonckheereTerpstra$new,

    multicomp.t = MultiCompT$new,
    multicomp.tukey = TukeyHSD$new,

    paired.comparison = PairedComparison$new,
    paired.sign = Sign$new,
    paired.signedscore = SignedScore$new,

    rcbd.anova = RCBDANOVA$new,
    rcbd.friedman = Friedman$new,
    rcbd.page = Page$new,

    association.corr = Correlation$new,

    table.chi = ChiSquare$new
))

tests_df <- data.frame(
    key = names(tests),
    test = unname(sapply(
        tests, function(new) class(do.call(new, list()))[[1]]
    ))
)

#' @title Syntactic Sugar for Object Construction
#' 
#' @description Create a test object conveniently. 
#' 
#' @name pmt


#' @rdname pmt
#' 
#' @param key a character string corresponding to the desired test. Check `pmts(...)` to see available keys. 
#' @param ... extra parameters passed to the initialize method of the test class. 
#' 
#' @export
pmt <- function(key, ...) do.call(key, list(...), envir = tests)

#' @rdname pmt
#' 
#' @param category a character string specifying which tests to show. 
#' 
#' @export
pmts <- function(category = c("all", "onesample", "twosample", "ksample", "multicomp", "paired", "rcbd", "association", "table")) {
    category <- match.arg(category)

    if (category == "all") tests_df else tests_df[startsWith(tests_df$key, category), ]
}