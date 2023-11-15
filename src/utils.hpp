#ifndef UTILS_H
#define UTILS_H

#include <Rcpp.h>

inline int rand_int(const int n)
{
    return floor(unif_rand() * n);
}

int n_combination(int n, int k);

template <typename Vec>
int n_permutation(Vec v)
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

#endif
