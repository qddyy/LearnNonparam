#pragma once

#include <Rcpp.h>
#include <sstream>

using namespace Rcpp;

class PermuBarBase {
public:
    NumericVector statistic_permu;

protected:
    NumericVector::iterator _iter;
    NumericVector::iterator _end;

    void _init_statistic_permu(R_xlen_t n_permu, R_len_t statistic_size)
    {
        statistic_permu = NumericVector(no_init(n_permu * statistic_size));

        _iter = statistic_permu.begin();
        _end = statistic_permu.end();

        if (statistic_size > 1) {
            statistic_permu.attr("dim") = IntegerVector::create(statistic_size, n_permu);
        }
    }
};

class PermuBarDisappear : public PermuBarBase {
public:
    PermuBarDisappear(R_xlen_t n_permu, bool exact, R_len_t statistic_size = 1)
    {
        _init_statistic_permu(n_permu, statistic_size);
    }

    bool update(double statistic)
    {
        *_iter = statistic;

        _iter++;

        return _iter != _end;
    }
};

class PermuBarAppear : public PermuBarBase {
public:
    PermuBarAppear(R_xlen_t n_permu, bool exact, R_len_t statistic_size = 1)
    {
        _init_statistic_permu(n_permu, statistic_size);

        _total = statistic_permu.size();

        if (_total < 100) {
            update_every = 1;
        } else {
            update_every = _total / 100;
        }

        if (exact) {
            _label = "Building exact permutation distribution";
        } else {
            _label = "Sampling from exact permutation distribution";
        }

        _print();
    }

    ~PermuBarAppear()
    {
        _print();
        Rcout << "\015\033[K\033[0m";
    }

    bool update(double statistic)
    {
        *_iter = statistic;

        update_i++;
        if (update_i == update_every) {
            update_i = 0;
            _print();
        }

        _iter++;

        return _iter != _end;
    }

private:
    R_xlen_t _total;

    R_xlen_t update_i = 0;
    R_xlen_t update_every;

    std::string _label;

    void _print()
    {
        std::ostringstream buffer;

        buffer << "\015";

        unsigned percent = 100 - static_cast<unsigned>(100 * (_end - _iter) / _total);

        buffer << "\033[31m" << percent << "%";

        buffer << " ";

        buffer << "\033[32m" << "[";
        unsigned n_fill = (percent >> 2);
        unsigned i = 0;
        while (i < n_fill) {
            buffer << "=";
            i++;
        }
        while (i < 25) {
            buffer << " ";
            i++;
        }
        buffer << "]";

        buffer << " ";

        buffer << "\033[34m" << _label;

        Rcout << buffer.str();
    }
};