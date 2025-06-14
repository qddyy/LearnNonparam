#include <Rcpp/Lightest>

using namespace Rcpp;

template <unsigned n>
constexpr auto Rf_lang = nullptr;

template <>
constexpr auto Rf_lang<2> = Rf_lang2;

template <>
constexpr auto Rf_lang<3> = Rf_lang3;

template <typename T>
class CachedFunc : public Function {
public:
    using Function::Function;

    template <typename... Args>
    auto operator()(Args&&... args) const
    {
        return [R_call = Shield<SEXP>(Rf_lang<sizeof...(args) + 1>(Function::operator()(std::forward<Args>(args)...), std::forward<Args>(args)...))](auto&&...) {
            return as<T>(Rcpp_fast_eval(R_call, R_GlobalEnv));
        };
    }
};

template <typename T>
class TypedFunc : public Function {
public:
    using Function::Function;

    template <typename... Args>
    T operator()(Args&&... args) const
    {
        return as<T>(Function::operator()(std::forward<Args>(args)...));
    }
};

#include "pmt/permutation.hpp"
#include "pmt/progress.hpp"

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
        impl_twosample_pmt<true, CachedFunc<double>>(clone(x), clone(y), statistic_func, n_permu) :
        impl_twosample_pmt<false, CachedFunc<double>>(clone(x), clone(y), statistic_func, n_permu);
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
        impl_ksample_pmt<true, CachedFunc<double>>(data, clone(group), statistic_func, n_permu) :
        impl_ksample_pmt<false, CachedFunc<double>>(data, clone(group), statistic_func, n_permu);
}

#include "pmt/impl_multcomp_pmt.hpp"

// [[Rcpp::export]]
SEXP multcomp_pmt(
    const SEXP data,
    const SEXP group,
    const SEXP statistic_func,
    const double n_permu,
    const bool progress)
{
    return progress ?
        impl_multcomp_pmt<true, CachedFunc<TypedFunc<double>>>(data, clone(group), statistic_func, n_permu) :
        impl_multcomp_pmt<false, CachedFunc<TypedFunc<double>>>(data, clone(group), statistic_func, n_permu);
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
        impl_paired_pmt<true, CachedFunc<double>>(clone(x), clone(y), statistic_func, n_permu) :
        impl_paired_pmt<false, CachedFunc<double>>(clone(x), clone(y), statistic_func, n_permu);
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
        impl_rcbd_pmt<true, CachedFunc<double>>(clone(data), statistic_func, n_permu) :
        impl_rcbd_pmt<false, CachedFunc<double>>(clone(data), statistic_func, n_permu);
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
        impl_association_pmt<true, CachedFunc<double>>(clone(x), clone(y), statistic_func, n_permu) :
        impl_association_pmt<false, CachedFunc<double>>(clone(x), clone(y), statistic_func, n_permu);
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
        impl_table_pmt<true, CachedFunc<double>>(clone(row), clone(col), statistic_func, n_permu) :
        impl_table_pmt<false, CachedFunc<double>>(clone(row), clone(col), statistic_func, n_permu);
}