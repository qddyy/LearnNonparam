// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// twosample_pmt
NumericVector twosample_pmt(const NumericVector x, const NumericVector y, const Function statistic_func, const double n_permu, const bool progress);
RcppExport SEXP _LearnNonparam_twosample_pmt(SEXP xSEXP, SEXP ySEXP, SEXP statistic_funcSEXP, SEXP n_permuSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< const NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< const Function >::type statistic_func(statistic_funcSEXP);
    Rcpp::traits::input_parameter< const double >::type n_permu(n_permuSEXP);
    Rcpp::traits::input_parameter< const bool >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(twosample_pmt(x, y, statistic_func, n_permu, progress));
    return rcpp_result_gen;
END_RCPP
}
// ksample_pmt
NumericVector ksample_pmt(const NumericVector data, const IntegerVector group, const Function statistic_func, const double n_permu, const bool progress);
RcppExport SEXP _LearnNonparam_ksample_pmt(SEXP dataSEXP, SEXP groupSEXP, SEXP statistic_funcSEXP, SEXP n_permuSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericVector >::type data(dataSEXP);
    Rcpp::traits::input_parameter< const IntegerVector >::type group(groupSEXP);
    Rcpp::traits::input_parameter< const Function >::type statistic_func(statistic_funcSEXP);
    Rcpp::traits::input_parameter< const double >::type n_permu(n_permuSEXP);
    Rcpp::traits::input_parameter< const bool >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(ksample_pmt(data, group, statistic_func, n_permu, progress));
    return rcpp_result_gen;
END_RCPP
}
// multcomp_pmt
NumericVector multcomp_pmt(const IntegerVector group_i, const IntegerVector group_j, const NumericVector data, const IntegerVector group, const Function statistic_func, const double n_permu, const bool progress);
RcppExport SEXP _LearnNonparam_multcomp_pmt(SEXP group_iSEXP, SEXP group_jSEXP, SEXP dataSEXP, SEXP groupSEXP, SEXP statistic_funcSEXP, SEXP n_permuSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const IntegerVector >::type group_i(group_iSEXP);
    Rcpp::traits::input_parameter< const IntegerVector >::type group_j(group_jSEXP);
    Rcpp::traits::input_parameter< const NumericVector >::type data(dataSEXP);
    Rcpp::traits::input_parameter< const IntegerVector >::type group(groupSEXP);
    Rcpp::traits::input_parameter< const Function >::type statistic_func(statistic_funcSEXP);
    Rcpp::traits::input_parameter< const double >::type n_permu(n_permuSEXP);
    Rcpp::traits::input_parameter< const bool >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(multcomp_pmt(group_i, group_j, data, group, statistic_func, n_permu, progress));
    return rcpp_result_gen;
END_RCPP
}
// paired_pmt
NumericVector paired_pmt(const NumericVector x, const NumericVector y, const Function statistic_func, const double n_permu, const bool progress);
RcppExport SEXP _LearnNonparam_paired_pmt(SEXP xSEXP, SEXP ySEXP, SEXP statistic_funcSEXP, SEXP n_permuSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< const NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< const Function >::type statistic_func(statistic_funcSEXP);
    Rcpp::traits::input_parameter< const double >::type n_permu(n_permuSEXP);
    Rcpp::traits::input_parameter< const bool >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(paired_pmt(x, y, statistic_func, n_permu, progress));
    return rcpp_result_gen;
END_RCPP
}
// rcbd_pmt
NumericVector rcbd_pmt(const NumericMatrix data, const Function statistic_func, const double n_permu, const bool progress);
RcppExport SEXP _LearnNonparam_rcbd_pmt(SEXP dataSEXP, SEXP statistic_funcSEXP, SEXP n_permuSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< const Function >::type statistic_func(statistic_funcSEXP);
    Rcpp::traits::input_parameter< const double >::type n_permu(n_permuSEXP);
    Rcpp::traits::input_parameter< const bool >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(rcbd_pmt(data, statistic_func, n_permu, progress));
    return rcpp_result_gen;
END_RCPP
}
// association_pmt
NumericVector association_pmt(const NumericVector x, const NumericVector y, const Function statistic_func, const double n_permu, const bool progress);
RcppExport SEXP _LearnNonparam_association_pmt(SEXP xSEXP, SEXP ySEXP, SEXP statistic_funcSEXP, SEXP n_permuSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< const NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< const Function >::type statistic_func(statistic_funcSEXP);
    Rcpp::traits::input_parameter< const double >::type n_permu(n_permuSEXP);
    Rcpp::traits::input_parameter< const bool >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(association_pmt(x, y, statistic_func, n_permu, progress));
    return rcpp_result_gen;
END_RCPP
}
// table_pmt
NumericVector table_pmt(const IntegerVector row, const IntegerVector col, const Function statistic_func, const double n_permu, const bool progress);
RcppExport SEXP _LearnNonparam_table_pmt(SEXP rowSEXP, SEXP colSEXP, SEXP statistic_funcSEXP, SEXP n_permuSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const IntegerVector >::type row(rowSEXP);
    Rcpp::traits::input_parameter< const IntegerVector >::type col(colSEXP);
    Rcpp::traits::input_parameter< const Function >::type statistic_func(statistic_funcSEXP);
    Rcpp::traits::input_parameter< const double >::type n_permu(n_permuSEXP);
    Rcpp::traits::input_parameter< const bool >::type progress(progressSEXP);
    rcpp_result_gen = Rcpp::wrap(table_pmt(row, col, statistic_func, n_permu, progress));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_LearnNonparam_twosample_pmt", (DL_FUNC) &_LearnNonparam_twosample_pmt, 5},
    {"_LearnNonparam_ksample_pmt", (DL_FUNC) &_LearnNonparam_ksample_pmt, 5},
    {"_LearnNonparam_multcomp_pmt", (DL_FUNC) &_LearnNonparam_multcomp_pmt, 7},
    {"_LearnNonparam_paired_pmt", (DL_FUNC) &_LearnNonparam_paired_pmt, 5},
    {"_LearnNonparam_rcbd_pmt", (DL_FUNC) &_LearnNonparam_rcbd_pmt, 4},
    {"_LearnNonparam_association_pmt", (DL_FUNC) &_LearnNonparam_association_pmt, 5},
    {"_LearnNonparam_table_pmt", (DL_FUNC) &_LearnNonparam_table_pmt, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_LearnNonparam(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
