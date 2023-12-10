#ifndef UTILS_H
#define UTILS_H

#include <Rcpp.h>
#include <algorithm>
#include <string>
#include <tuple>

using namespace Rcpp;

// progress bar

class ProgressBar {
private:
    unsigned _end;

    unsigned _update_every;

    std::string _label;

    bool _appear;

public:
    ProgressBar() {}

    ProgressBar(unsigned n, bool appear)
    {
        _end = n - 1;
        _update_every = 1 + _end / 100;

        _appear = appear;
    }

    void set_label(std::string label)
    {
        _label = label;
    }

    void done()
    {
        if (_appear) {
            Rcout << "\015" << "\033[K" << "\033[0m";
        }
    }

    void update(unsigned current)
    {
        if (_appear && (current % _update_every == 0)) {
            Rcout << "\015";

            unsigned percent = 100 * current / _end;

            Rcout << "\033[31m" << percent << "%";

            Rcout << " ";

            Rcout << "\033[32m" << "[";
            unsigned n_fill = percent >> 2;
            unsigned i = 0;
            while (i < n_fill) {
                Rcout << "=";
                i++;
            }
            while (i < 25) {
                Rcout << " ";
                i++;
            }
            Rcout << "]";

            Rcout << " ";

            Rcout << "\033[34m" << _label;
        }
    }
};

std::tuple<NumericVector, ProgressBar> statistic_permu_with_bar(
    const unsigned n, const bool exact,
    const unsigned statistic_size = 1);

// random shuffle (tied to the same RNG which R uses)

inline unsigned rand_int(const unsigned& n)
{
    return floor(unif_rand() * n);
}

template <typename T>
void random_shuffle(T&& v)
{
    unsigned j;
    unsigned n = v.size();
    for (unsigned i = 0; i < n - 1; i++) {
        j = i + rand_int(n - i);
        std::swap(v[i], v[j]);
    }
}

// count

template <typename T>
unsigned n_permutation(T&& v)
{
    double A = 1;

    unsigned n_i = 0;
    unsigned n = v.size();
    double current = v[0];
    for (unsigned i = 0; i < n; i++) {
        A *= (i + 1);
        if (v[i] == current) {
            n_i++;
            A /= n_i;
        } else {
            n_i = 1;
        }
        current = v[i];
    }

    return (unsigned)A;
}

#endif
