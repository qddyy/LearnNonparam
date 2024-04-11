#pragma once

#include <Rcpp.h>
#include <sstream>

using namespace Rcpp;

template <typename T>
class PermuBarBase {
private:
    T& _Derived()
    {
        return static_cast<T&>(*this);
    }

public:
    void init(R_xlen_t n_permu, bool exact, R_len_t statistic_size = 1)
    {
        _Derived().init_impl(n_permu, exact, statistic_size);
    }

    bool update(double statistic)
    {
        return _Derived().update_impl(statistic);
    }

    bool update(SEXP statistic)
    {
        return _Derived().update_impl(as<double>(statistic));
    }

    NumericVector close()
    {
        return _Derived().close_impl();
    }

protected:
    NumericVector statistic_permu;

    NumericVector::iterator _iter;
    NumericVector::iterator _end;

    void _init_impl_statistic_permu(R_xlen_t n_permu, R_len_t statistic_size)
    {
        statistic_permu = NumericVector(no_init(n_permu * statistic_size));

        _iter = statistic_permu.begin();
        _end = statistic_permu.end();

        if (statistic_size > 1) {
            statistic_permu.attr("dim") = IntegerVector::create(statistic_size, n_permu);
        }
    }

    bool _update_double(double statistic) {
        *_iter = statistic;

        _iter++;

        return _iter != _end;
    }
};

class PermuBarDisappear : public PermuBarBase<PermuBarDisappear> {
public:
    void init_impl(R_xlen_t n_permu, bool exact, R_len_t statistic_size)
    {
        _init_impl_statistic_permu(n_permu, statistic_size);
    }

    bool update_impl(double statistic)
    {
        return _update_double(statistic);
    }

    NumericVector close_impl()
    {
        return statistic_permu;
    }
};

class PermuBarAppear : public PermuBarBase<PermuBarAppear> {
public:
    void init_impl(R_xlen_t n_permu, bool exact, R_len_t statistic_size)
    {
        _init_impl_statistic_permu(n_permu, statistic_size);

        _total = statistic_permu.size();

        _update_every = (_total < 100) ? 1 : _total / 100;

        _label = (exact) ?
            "Building exact permutation distribution" :
            "Sampling from exact permutation distribution";

        _print();
    }

    bool update_impl(double statistic)
    {
        _update_i++;
        if (_update_i == _update_every) {
            _update_i = 0;
            _print();
        }

        return _update_double(statistic);
    }

    NumericVector close_impl()
    {
        _print();
        Rcout << "\015\033[K\033[0m";

        return statistic_permu;
    }

private:
    R_xlen_t _total;

    R_xlen_t _update_i = 0;
    R_xlen_t _update_every;

    std::string _label;

    void _print()
    {
        unsigned percent = 100 - static_cast<unsigned>(100 * (_end - _iter) / _total);

        unsigned n_fill = (percent >> 2);

        std::ostringstream buffer;

        buffer << "\015\033[31m" << percent << "% \033[32m[";
        unsigned i = 0;
        while (i < n_fill) {
            buffer << "=";
            i++;
        }
        while (i < 25) {
            buffer << " ";
            i++;
        }
        buffer << "] \033[34m" << _label;

        Rcout << buffer.str();
    }
};