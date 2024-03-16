#pragma once

#include <Rcpp.h>
#include <algorithm>

using namespace Rcpp;

// runif(max = n) (tied to the same RNG which R uses)

template <typename T>
T rand_int(T n)
{
    return floor(unif_rand() * n);
}

// random shuffle

template <typename T>
void random_shuffle(T&& v)
{
    R_len_t n = v.size();
    for (R_len_t i = 0; i < n - 1; i++) {
        R_len_t j = i + rand_int(n - i);
        std::swap(v[i], v[j]);
    }
}

// number of permutations

template <typename T>
R_xlen_t n_permutation(T&& v)
{
    double A = 1;

    R_len_t n = v.size();
    R_len_t n_i = 0;
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

    return static_cast<R_xlen_t>(A);
}