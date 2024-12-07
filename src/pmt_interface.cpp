#include <Rcpp/Lightest>

using namespace Rcpp;

#include "pmt/progress.hpp"
#include "pmt/reorder.hpp"

class StatFunc : public Function {
public:
    using Function::Function;

    template <typename... Args>
    auto operator()(Args&&... args) const
    {
        return [r_closure = Function(Function::operator()(std::forward<Args>(args)...))](auto&&... args) {
            return as<double>(r_closure(std::forward<decltype(args)>(args)...));
        };
    }
};

#include "pmt/impl_twosample_pmt.hpp"

// [[Rcpp::export]]
SEXP twosample_pmt(
    const SEXP x,
    const SEXP y,
    const SEXP statistic_func,
    const double n_permu,
    const bool progress)
{
    return progress ?
        impl_twosample_pmt<PermuBarShow, StatFunc>(clone(x), clone(y), statistic_func, n_permu) :
        impl_twosample_pmt<PermuBarHide, StatFunc>(clone(x), clone(y), statistic_func, n_permu);
}

#include "pmt/impl_ksample_pmt.hpp"

// [[Rcpp::export]]
SEXP ksample_pmt(
    const SEXP data,
    const SEXP group,
    const SEXP statistic_func,
    const double n_permu,
    const bool progress)
{
    return progress ?
        impl_ksample_pmt<PermuBarShow, StatFunc>(data, clone(group), statistic_func, n_permu) :
        impl_ksample_pmt<PermuBarHide, StatFunc>(data, clone(group), statistic_func, n_permu);
}

#include "pmt/impl_multcomp_pmt.hpp"

// [[Rcpp::export]]
SEXP multcomp_pmt(
    const SEXP group_i,
    const SEXP group_j,
    const SEXP data,
    const SEXP group,
    const SEXP statistic_func,
    const double n_permu,
    const bool progress)
{
    return progress ?
        impl_multcomp_pmt<PermuBarShow, StatFunc>(group_i, group_j, data, clone(group), statistic_func, n_permu) :
        impl_multcomp_pmt<PermuBarHide, StatFunc>(group_i, group_j, data, clone(group), statistic_func, n_permu);
}

#include "pmt/impl_paired_pmt.hpp"

// [[Rcpp::export]]
SEXP paired_pmt(
    const SEXP x,
    const SEXP y,
    const SEXP statistic_func,
    const double n_permu,
    const bool progress)
{
    return progress ?
        impl_paired_pmt<PermuBarShow, StatFunc>(clone(x), clone(y), statistic_func, n_permu) :
        impl_paired_pmt<PermuBarHide, StatFunc>(clone(x), clone(y), statistic_func, n_permu);
}

#include "pmt/impl_rcbd_pmt.hpp"

// [[Rcpp::export]]
SEXP rcbd_pmt(
    const SEXP data,
    const SEXP statistic_func,
    const double n_permu,
    const bool progress)
{
    return progress ?
        impl_rcbd_pmt<PermuBarShow, StatFunc>(clone(data), statistic_func, n_permu) :
        impl_rcbd_pmt<PermuBarHide, StatFunc>(clone(data), statistic_func, n_permu);
}

#include "pmt/impl_association_pmt.hpp"

// [[Rcpp::export]]
SEXP association_pmt(
    const SEXP x,
    const SEXP y,
    const SEXP statistic_func,
    const double n_permu,
    const bool progress)
{
    return progress ?
        impl_association_pmt<PermuBarShow, StatFunc>(x, clone(y), statistic_func, n_permu) :
        impl_association_pmt<PermuBarHide, StatFunc>(x, clone(y), statistic_func, n_permu);
}

#include "pmt/impl_table_pmt.hpp"

// [[Rcpp::export]]
SEXP table_pmt(
    const SEXP row,
    const SEXP col,
    const SEXP statistic_func,
    const double n_permu,
    const bool progress)
{
    return progress ?
        impl_table_pmt<PermuBarShow, StatFunc>(row, clone(col), statistic_func, n_permu) :
        impl_table_pmt<PermuBarHide, StatFunc>(row, clone(col), statistic_func, n_permu);
}
