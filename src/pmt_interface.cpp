#include <Rcpp.h>

using namespace Rcpp;

#include "pmt/macros.hpp"
#include "pmt/progress.hpp"
#include "pmt/reorder.hpp"

class ClosFunc {
private:
    Function _func;

public:
    ClosFunc(Function func) :
        _func(func) { }

    template <typename... Args>
    auto operator()(Args... args) const
    {
        Function closure = _func(args...);

        return [closure](auto... args_) {
            return as<double>(closure(args_...));
        };
    }
};

#include "pmt/impl_twosample_pmt.hpp"

// [[Rcpp::export]]
NumericVector twosample_pmt(
    const NumericVector x,
    const NumericVector y,
    const Function statistic,
    const R_xlen_t n_permu,
    const bool progress)
{
    ClosFunc statistic_func(statistic);
    PMT_PROGRESS_RETURN(impl_twosample_pmt, x, y)
}

#include "pmt/impl_ksample_pmt.hpp"

// [[Rcpp::export]]
NumericVector ksample_pmt(
    const NumericVector data,
    const IntegerVector group,
    const Function statistic,
    const R_xlen_t n_permu,
    const bool progress)
{
    ClosFunc statistic_func(statistic);
    PMT_PROGRESS_RETURN(impl_ksample_pmt, data, group)
}

#include "pmt/impl_multcomp_pmt.hpp"

// [[Rcpp::export]]
NumericVector multcomp_pmt(
    const IntegerVector group_i,
    const IntegerVector group_j,
    const NumericVector data,
    const IntegerVector group,
    const Function statistic,
    const R_xlen_t n_permu,
    const bool progress)
{
    ClosFunc statistic_func(statistic);
    PMT_PROGRESS_RETURN(impl_multcomp_pmt, group_i, group_j, data, group)
}

#include "pmt/impl_paired_pmt.hpp"

// [[Rcpp::export]]
NumericVector paired_pmt(
    const NumericVector x,
    const NumericVector y,
    const Function statistic,
    const R_xlen_t n_permu,
    const bool progress)
{
    ClosFunc statistic_func(statistic);
    PMT_PROGRESS_RETURN(impl_paired_pmt, x, y)
}

#include "pmt/impl_rcbd_pmt.hpp"

// [[Rcpp::export]]
NumericVector rcbd_pmt(
    const NumericMatrix data,
    const Function statistic,
    const R_xlen_t n_permu,
    const bool progress)
{
    ClosFunc statistic_func(statistic);
    PMT_PROGRESS_RETURN(impl_rcbd_pmt, data)
}

#include "pmt/impl_association_pmt.hpp"

// [[Rcpp::export]]
NumericVector association_pmt(
    const NumericVector x,
    const NumericVector y,
    const Function statistic,
    const R_xlen_t n_permu,
    const bool progress)
{
    ClosFunc statistic_func(statistic);
    PMT_PROGRESS_RETURN(impl_association_pmt, x, y)
}

#include "pmt/impl_table_pmt.hpp"

// [[Rcpp::export]]
NumericVector table_pmt(
    const IntegerVector row_loc,
    const IntegerVector col_loc,
    const Function statistic,
    const R_xlen_t n_permu,
    const bool progress)
{
    ClosFunc statistic_func(statistic);
    PMT_PROGRESS_RETURN(impl_table_pmt, row_loc, col_loc)
}