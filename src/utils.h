#pragma once

#include <Rcpp.h>
#include <algorithm>

inline int rand_int(const int n)
{
    return floor(unif_rand() * n);
}

template <typename T>
void random_shuffle(T v)
{
    int j;
    int n = v.size();
    for (int i = 0; i < n - 1; i++) {
        j = i + rand_int(n - i);
        std::swap(v[i], v[j]);
    }
}

template <typename T>
int n_permutation(T v)
{
    double A = 1;

    int n_i = 0;
    int n = v.size();
    double current = v[0];
    for (int i = 0; i < n; i++) {
        A *= (i + 1);
        if (v[i] == current) {
            n_i++;
            A /= n_i;
        } else {
            n_i = 1;
        }
        current = v[i];
    }

    return (int)A;
}
