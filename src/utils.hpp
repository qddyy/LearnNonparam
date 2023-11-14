#ifndef UTILS_H
#define UTILS_H

#include <Rcpp.h>

inline int rand_int(const int n)
{
    return floor(unif_rand() * n);
}

int n_combination(int n, int k);

template <typename T>
int n_permutation(T x)
{
    double A = 1;

    int n_i = 0;
    int n = x.size();
    double current = x[0];
    for (int i = 0; i < n; i++) {
        A *= (i + 1);
        if (x[i] == current) {
            n_i++;
            A /= n_i;
        } else {
            n_i = 1;
        }
        current = x[i];
    }

    return (int)A;
}

#endif
