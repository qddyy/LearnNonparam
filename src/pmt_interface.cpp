#include <csetjmp>
#include <initializer_list>

#define RCPP_NO_BOUNDS_CHECK
#include <Rcpp.h>

using namespace Rcpp;

template <typename T>
class CachedFunc : public Function {
public:
    RObject token;

    std::jmp_buf jmpbuf;

    CachedFunc(SEXP func) :
        Function(func),
        token(R_MakeUnwindCont()) { }

    template <typename... Args>
    auto operator()(Args&&... args) const
    {
        Shield<SEXP> closure(Function::operator()(std::forward<Args>(args)...));

#if defined(R_VERSION) && R_VERSION >= R_Version(4, 5, 0)
        Shield<SEXP> closure_formals(R_ClosureFormals(closure)), closure_body(R_ClosureBody(closure)), closure_envir(R_ClosureEnv(closure));
#else
        Shield<SEXP> closure_formals(FORMALS(closure)), closure_body(BODY(closure)), closure_envir(CLOENV(closure));
#endif

#if defined(R_VERSION) && R_VERSION >= R_Version(4, 1, 0)
        Shield<SEXP> exec_envir(R_NewEnv(closure_envir, FALSE, 0));
#else
        Shield<SEXP> exec_envir(Rf_allocSExp(ENVSXP));
        SET_ENCLOS(exec_envir, closure_envir);
#endif

        SEXP f = closure_formals;
        (void)std::initializer_list<int> { (Rf_defineVar(TAG(f), std::forward<Args>(args), exec_envir), f = CDR(f), 0)... };

        auto cached = [closure_body = RObject(closure_body), exec_envir = RObject(exec_envir)]() {
            return Rf_eval(closure_body, exec_envir);
        };

        auto exec = +[](void* cached_ptr) { return (*static_cast<decltype(cached)*>(cached_ptr))(); };
        auto jump = +[](void* jmpbuf_ptr, Rboolean jump) { if (jump) longjmp(*static_cast<std::jmp_buf*>(jmpbuf_ptr), 1); };
        return [cached = std::move(cached), exec, jump, this](auto&&...) {
            return as<T>(R_UnwindProtect(
                exec, const_cast<void*>(static_cast<const void*>(&cached)),
                jump, const_cast<void*>(static_cast<const void*>(&this->jmpbuf)),
                this->token));
        };
    }
};

#define SETJMP(cached_func)                         \
    if (setjmp(cached_func.jmpbuf)) {               \
        throw LongjumpException(cached_func.token); \
    }

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

class MultcompFunc : public CachedFunc<SEXP> {
public:
    using CachedFunc<SEXP>::CachedFunc;

    template <typename... Args>
    auto operator()(Args&&... args) const
    {
        return [statistic_closure = CachedFunc<SEXP>::operator()(std::forward<Args>(args)...)](auto&&... args) {
            return REAL(statistic_closure(std::forward<decltype(args)>(args)...));
        };
    }
};

// [[Rcpp::export]]
SEXP multcomp_pmt(
    const SEXP data,
    const SEXP group,
    const SEXP statistic_func,
    const double n_permu,
    const bool progress)
{
    return progress ?
        impl_multcomp_pmt<true, MultcompFunc>(data, clone(group), statistic_func, n_permu) :
        impl_multcomp_pmt<false, MultcompFunc>(data, clone(group), statistic_func, n_permu);
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
    const SEXP data,
    const SEXP statistic_func,
    const double n_permu,
    const bool progress)
{
    return progress ?
        impl_table_pmt<true, CachedFunc<double>>(clone(data), statistic_func, n_permu) :
        impl_table_pmt<false, CachedFunc<double>>(clone(data), statistic_func, n_permu);
}

// [[Rcpp::export]]
SEXP distribution_pmt(
    const SEXP x,
    const SEXP y,
    const SEXP statistic_func,
    const double n_permu,
    const bool progress)
{
    return progress ?
        impl_distribution_pmt<true, CachedFunc<double>>(x, y, statistic_func, n_permu) :
        impl_distribution_pmt<false, CachedFunc<double>>(x, y, statistic_func, n_permu);
}