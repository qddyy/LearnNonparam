#pragma once

#include <Rcpp.h>
#include <algorithm>

#include "progress.h"

using namespace Rcpp;

#define GENERATE_PMT_BODY(interface, ...)                                     \
    Function statistic_r(statistic_func);                                     \
    return (progress) ?                                                       \
        interface##_impl<PermuBarAppear>(__VA_ARGS__, statistic_r, n_permu) : \
        interface##_impl<PermuBarDisappear>(__VA_ARGS__, statistic_r, n_permu);

template <typename T>
T rand_int(T n)
{
    return floor(unif_rand() * n);
}

template <typename T>
void random_shuffle(T&& v)
{
    R_len_t n = v.size();
    for (R_len_t i = 0; i < n - 1; i++) {
        R_len_t j = i + rand_int(n - i);
        std::swap(v[i], v[j]);
    }
}

template <typename T>
bool next_permutation(T&& v)
{
    return std::next_permutation(v.begin(), v.end());
}

template <typename T>
R_xlen_t n_permutation(const T& v)
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