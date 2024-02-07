#ifndef UTILS_H
#define UTILS_H

#include <Rcpp.h>
#include <algorithm>
#include <sstream>
#include <string>

using namespace Rcpp;

// runif(max = n) (tied to the same RNG which R uses)

template <typename T>
T rand_int(T n)
{
    return floor(unif_rand() * n);
}

// random shuffle

template <typename T>
void random_shuffle(T&& v, R_len_t v_size)
{
    R_len_t j;
    for (R_len_t i = 0; i < v_size - 1; i++) {
        j = i + rand_int(v_size - i);
        std::swap(v[i], v[j]);
    }
}

// number of permutations

template <typename T>
R_xlen_t n_permutation(T&& v)
{
    double A = 1;

    R_len_t n_i = 0;
    R_len_t n = v.size();
    double current = v[0];
    for (R_len_t i = 0; i < n; i++) {
        A *= (i + 1);
        if (v[i] == current) {
            n_i++;
            A /= n_i;
        } else {
            n_i = 1;
        }
        current = v[i];
    }

    return (R_xlen_t)A;
}

// progress bar

class PermuBar {
private:
    R_xlen_t _total;

    R_xlen_t _update_i = 0;
    R_xlen_t _update_every;

    NumericVector::iterator _iter;
    NumericVector::iterator _end;

    std::string _label;

    bool _appear;

    void _print()
    {
        std::ostringstream buffer;

        buffer << "\015";

        unsigned percent = 100 - (unsigned)(100 * (_end - _iter) / _total);

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

public:
    NumericVector statistic_permu;

    PermuBar(R_xlen_t n_permu, bool exact, R_len_t statistic_size = 1)
    {
        _total = n_permu * statistic_size;

        if (_total < 100) {
            _update_every = 1;
        } else {
            _update_every = _total / 100;
        }

        statistic_permu = NumericVector(no_init(_total));
        if (statistic_size > 1) {
            statistic_permu.attr("dim") = IntegerVector::create(statistic_size, n_permu);
        }

        _iter = statistic_permu.begin();
        _end = statistic_permu.end();

        if (exact) {
            _label = "Building exact permutation distribution";
        } else {
            _label = "Sampling from exact permutation distribution";
        }

        Environment base = Environment::base_env();
        Function interactive = base["interactive"];
        _appear = as<bool>(interactive());

        if (_appear) {
            _print();
        }
    }

    ~PermuBar()
    {
        if (_appear) {
            _print();
            Rcout << "\015\033[K\033[0m";
        }
    }

    bool update(double statistic)
    {
        *_iter = statistic;

        if (_appear) {
            _update_i++;
            if (_update_i == _update_every) {
                _update_i = 0;
                _print();
            }
        }

        _iter++;

        return _iter != _end;
    }
};

#endif
