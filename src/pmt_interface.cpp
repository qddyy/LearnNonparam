#include <Rcpp.h>

using namespace Rcpp;

#include "pmt/pmt_macros.hpp"
#include "pmt/pmt_progress.hpp"
#include "pmt/pmt_reorder.hpp"

#include "pmt/impl_twosample_pmt.hpp"

// [[Rcpp::export]]
NumericVector twosample_pmt(
    const NumericVector x,
    const NumericVector y,
    const Function statistic_func,
    const R_xlen_t n_permu,
    const bool progress)
{
    PMT_PROGRESS_RETURN(impl_twosample_pmt, Function, Function, x, y)
}

#include "pmt/impl_ksample_pmt.hpp"

// [[Rcpp::export]]
NumericVector ksample_pmt(
    const NumericVector data,
    const IntegerVector group,
    const Function statistic_func,
    const R_xlen_t n_permu,
    const bool progress)
{
    PMT_PROGRESS_RETURN(impl_ksample_pmt, Function, Function, data, group)
}

#include "pmt/impl_multcomp_pmt.hpp"

// [[Rcpp::export]]
NumericVector multcomp_pmt(
    const IntegerVector group_i,
    const IntegerVector group_j,
    const NumericVector data,
    const IntegerVector group,
    const Function statistic_func,
    const R_xlen_t n_permu,
    const bool progress)
{
    PMT_PROGRESS_RETURN(impl_multcomp_pmt, Function, Function, group_i, group_j, data, group)
}

#include "pmt/impl_paired_pmt.hpp"

// [[Rcpp::export]]
NumericVector paired_pmt(
    const NumericVector x,
    const NumericVector y,
    const Function statistic_func,
    const R_xlen_t n_permu,
    const bool progress)
{
    PMT_PROGRESS_RETURN(impl_paired_pmt, Function, Function, x, y)
}

#include "pmt/impl_rcbd_pmt.hpp"

// [[Rcpp::export]]
NumericVector rcbd_pmt(
    const NumericMatrix data,
    const Function statistic_func,
    const R_xlen_t n_permu,
    const bool progress)
{
    PMT_PROGRESS_RETURN(impl_rcbd_pmt, Function, Function, data)
}

#include "pmt/impl_association_pmt.hpp"

// [[Rcpp::export]]
NumericVector association_pmt(
    const NumericVector x,
    const NumericVector y,
    const Function statistic_func,
    const R_xlen_t n_permu,
    const bool progress) {
    PMT_PROGRESS_RETURN(impl_association_pmt, Function, Function, x, y)
}

#include "pmt/impl_table_pmt.hpp"

// [[Rcpp::export]]
NumericVector table_pmt(
    const IntegerVector row_loc,
    const IntegerVector col_loc,
    const Function statistic_func,
    const R_xlen_t n_permu,
    const bool progress)
{
    PMT_PROGRESS_RETURN(impl_table_pmt, Function, Function, row_loc, col_loc)
}
