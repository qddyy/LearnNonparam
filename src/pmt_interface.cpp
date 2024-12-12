#include <Rcpp/Lightest>

using namespace Rcpp;

#include "pmt/permutation.hpp"
#include "pmt/progress.hpp"

#include <type_traits>

template <bool sharing_args>
class StatFunc : public Function {
public:
    using Function::Function;

    template <typename... Args>
    auto operator()(Args&&... args) const
    {
        return _invoke(std::integral_constant<bool, sharing_args>(), std::forward<Args>(args)...);
    }

private:
    template <typename... Args>
    auto _invoke(std::false_type, Args&&... args) const
    {
        return [r_closure = Function(Function::operator()(std::forward<Args>(args)...))](auto&&... args) {
            return as<double>(r_closure(std::forward<decltype(args)>(args)...));
        };
    }

    template <typename... Args>
    auto _invoke(std::true_type, Args&&... args) const
    {
        return [r_call = Language(Function(Function::operator()(std::forward<Args>(args)...)), std::forward<Args>(args)...)](auto&&...) {
            return as<double>(r_call.eval());
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
        impl_twosample_pmt<true, StatFunc<true>>(clone(x), clone(y), statistic_func, n_permu) :
        impl_twosample_pmt<false, StatFunc<true>>(clone(x), clone(y), statistic_func, n_permu);
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
        impl_ksample_pmt<true, StatFunc<true>>(data, clone(group), statistic_func, n_permu) :
        impl_ksample_pmt<false, StatFunc<true>>(data, clone(group), statistic_func, n_permu);
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
        impl_multcomp_pmt<true, StatFunc<false>>(group_i, group_j, data, clone(group), statistic_func, n_permu) :
        impl_multcomp_pmt<false, StatFunc<false>>(group_i, group_j, data, clone(group), statistic_func, n_permu);
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
        impl_paired_pmt<true, StatFunc<true>>(clone(x), clone(y), statistic_func, n_permu) :
        impl_paired_pmt<false, StatFunc<true>>(clone(x), clone(y), statistic_func, n_permu);
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
        impl_rcbd_pmt<true, StatFunc<true>>(clone(data), statistic_func, n_permu) :
        impl_rcbd_pmt<false, StatFunc<true>>(clone(data), statistic_func, n_permu);
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
        impl_association_pmt<true, StatFunc<true>>(clone(x), clone(y), statistic_func, n_permu) :
        impl_association_pmt<false, StatFunc<true>>(clone(x), clone(y), statistic_func, n_permu);
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
        impl_table_pmt<true, StatFunc<true>>(clone(row), clone(col), statistic_func, n_permu) :
        impl_table_pmt<false, StatFunc<true>>(clone(row), clone(col), statistic_func, n_permu);
}