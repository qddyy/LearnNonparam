#include <Rcpp.h>

using namespace Rcpp;

#include "pmt/progress.hpp"
#include "pmt/reorder.hpp"

class ClosFunc : public Function {
public:
    using Function::Function;

    template <typename... Args>
    auto operator()(Args&&... args) const
    {
        return [closure = Function(Function::operator()(std::forward<Args>(args)...))](auto&&... args) {
            return as<double>(closure(std::forward<decltype(args)>(args)...));
        };
    }
};

#include "pmt/impl_twosample_pmt.hpp"

// [[Rcpp::export]]
NumericVector twosample_pmt(
    const NumericVector x,
    const NumericVector y,
    const Function statistic_func,
    const double n_permu,
    const bool progress)
{
    return progress ?
        impl_twosample_pmt<PermuBarShow>(clone(x), clone(y), ClosFunc(statistic_func), n_permu) :
        impl_twosample_pmt<PermuBarHide>(clone(x), clone(y), ClosFunc(statistic_func), n_permu);
}

#include "pmt/impl_ksample_pmt.hpp"

// [[Rcpp::export]]
NumericVector ksample_pmt(
    const NumericVector data,
    const IntegerVector group,
    const Function statistic_func,
    const double n_permu,
    const bool progress)
{
    return progress ?
        impl_ksample_pmt<PermuBarShow>(data, clone(group), ClosFunc(statistic_func), n_permu) :
        impl_ksample_pmt<PermuBarHide>(data, clone(group), ClosFunc(statistic_func), n_permu);
}

#include "pmt/impl_multcomp_pmt.hpp"

// [[Rcpp::export]]
NumericVector multcomp_pmt(
    const IntegerVector group_i,
    const IntegerVector group_j,
    const NumericVector data,
    const IntegerVector group,
    const Function statistic_func,
    const double n_permu,
    const bool progress)
{
    return progress ?
        impl_multcomp_pmt<PermuBarShow>(group_i, group_j, data, clone(group), ClosFunc(statistic_func), n_permu) :
        impl_multcomp_pmt<PermuBarHide>(group_i, group_j, data, clone(group), ClosFunc(statistic_func), n_permu);
}

#include "pmt/impl_paired_pmt.hpp"

// [[Rcpp::export]]
NumericVector paired_pmt(
    const NumericVector x,
    const NumericVector y,
    const Function statistic_func,
    const double n_permu,
    const bool progress)
{
    return progress ?
        impl_paired_pmt<PermuBarShow>(clone(x), clone(y), ClosFunc(statistic_func), n_permu) :
        impl_paired_pmt<PermuBarHide>(clone(x), clone(y), ClosFunc(statistic_func), n_permu);
}

#include "pmt/impl_rcbd_pmt.hpp"

// [[Rcpp::export]]
NumericVector rcbd_pmt(
    const NumericMatrix data,
    const Function statistic_func,
    const double n_permu,
    const bool progress)
{
    return progress ?
        impl_rcbd_pmt<PermuBarShow>(clone(data), ClosFunc(statistic_func), n_permu) :
        impl_rcbd_pmt<PermuBarHide>(clone(data), ClosFunc(statistic_func), n_permu);
}

#include "pmt/impl_association_pmt.hpp"

// [[Rcpp::export]]
NumericVector association_pmt(
    const NumericVector x,
    const NumericVector y,
    const Function statistic_func,
    const double n_permu,
    const bool progress)
{
    return progress ?
        impl_association_pmt<PermuBarShow>(x, clone(y), ClosFunc(statistic_func), n_permu) :
        impl_association_pmt<PermuBarHide>(x, clone(y), ClosFunc(statistic_func), n_permu);
}

#include "pmt/impl_table_pmt.hpp"

// [[Rcpp::export]]
NumericVector table_pmt(
    const IntegerVector row,
    const IntegerVector col,
    const Function statistic_func,
    const double n_permu,
    const bool progress)
{
    return progress ?
        impl_table_pmt<PermuBarShow>(row, clone(col), ClosFunc(statistic_func), n_permu) :
        impl_table_pmt<PermuBarHide>(row, clone(col), ClosFunc(statistic_func), n_permu);
}