Tests <- as.environment(list(
    onesample.quantile = Quantile$new,
    onesample.ecdf = ECDF$new,

    twosample.mean = MeanDiff$new,
    twosample.wilcoxon = Wilcoxon$new,
    twosample.scoresum = ScoreSum$new,
    twosample.st = SiegelTukey$new,
    twosample.ab = AnsariBradley$new,
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


#' @title Create a test object conveniently
#' 
#' @description This class specializes `PermuTest` for permutation tests for contingency tables. 
#' 
#' @param key key corresponding to the desired test class. Check `pmts` to see all keys available. 
#' @param ... extra parameters passed to the initialize method of the test class. 
#' 
#' @export pmt
pmt <- function(key, ...) do.call(key, list(...), envir = Tests)

#' @title All tests available. 
#' 
#' @export pmts
pmts <- data.frame(
    key = names(Tests),
    test = unname(sapply(Tests, function(new) class(do.call(new, list()))[[1]]))
)