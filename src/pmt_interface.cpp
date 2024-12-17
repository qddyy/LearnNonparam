#include <Rcpp/Lightest>

using namespace Rcpp;

#include "pmt/permutation.hpp"
#include "pmt/progress.hpp"

#include <type_traits>

template <unsigned i>
using tag_t = std::integral_constant<unsigned, i>;

template <unsigned n>
constexpr auto Rf_lang = nullptr;

template <>
constexpr auto Rf_lang<2> = Rf_lang2;

template <>
constexpr auto Rf_lang<3> = Rf_lang3;

template <unsigned n_shared>
class StatFunc : public Function {
public:
    using Function::Function;

    template <typename... Args>
    auto operator()(Args&&... args) const
    {
        return _invoke(tag_t<n_shared>(), std::forward<Args>(args)...);
    }

private:
    template <typename... Args>
    auto _invoke(tag_t<0>, Args&&... args) const
    {
        return [r_closure = Function(Function::operator()(std::forward<Args>(args)...))](auto&&... args) {
            return as<double>(r_closure(std::forward<decltype(args)>(args)...));
        };
    }

    template <unsigned n = n_shared, typename... Args>
    auto _invoke(tag_t<n>, Args&&... args) const
    {
        return [r_call = RObject(Rf_lang<n + 1>(Function::operator()(std::forward<Args>(args)...), std::forward<Args>(args)...))](auto&&...) {
            return as<double>(Rcpp_fast_eval(r_call, R_GlobalEnv));
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
        impl_twosample_pmt<true, StatFunc<2>>(clone(x), clone(y), statistic_func, n_permu) :
        impl_twosample_pmt<false, StatFunc<2>>(clone(x), clone(y), statistic_func, n_permu);
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
        impl_ksample_pmt<true, StatFunc<2>>(data, clone(group), statistic_func, n_permu) :
        impl_ksample_pmt<false, StatFunc<2>>(data, clone(group), statistic_func, n_permu);
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
        impl_multcomp_pmt<true, StatFunc<0>>(group_i, group_j, data, clone(group), statistic_func, n_permu) :
        impl_multcomp_pmt<false, StatFunc<0>>(group_i, group_j, data, clone(group), statistic_func, n_permu);
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
        impl_paired_pmt<true, StatFunc<2>>(clone(x), clone(y), statistic_func, n_permu) :
        impl_paired_pmt<false, StatFunc<2>>(clone(x), clone(y), statistic_func, n_permu);
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
        impl_rcbd_pmt<true, StatFunc<1>>(clone(data), statistic_func, n_permu) :
        impl_rcbd_pmt<false, StatFunc<1>>(clone(data), statistic_func, n_permu);
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
        impl_association_pmt<true, StatFunc<2>>(clone(x), clone(y), statistic_func, n_permu) :
        impl_association_pmt<false, StatFunc<2>>(clone(x), clone(y), statistic_func, n_permu);
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
        impl_table_pmt<true, StatFunc<1>>(clone(row), clone(col), statistic_func, n_permu) :
        impl_table_pmt<false, StatFunc<1>>(clone(row), clone(col), statistic_func, n_permu);
}