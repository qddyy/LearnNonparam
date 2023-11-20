#ifndef UTILS_H
#define UTILS_H

#include <Rcpp.h>
#include <algorithm>
#include <cli/progress.h>
#include <tuple>

using namespace Rcpp;

// progress bar

std::tuple<NumericVector, RObject> statistic_permu_with_bar(
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
