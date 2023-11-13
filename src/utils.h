#ifndef UTILS_H
#define UTILS_H

#include <Rcpp.h>

using namespace Rcpp;

inline int rand_int(const int n)
{
    return floor(unif_rand() * n);
}

int n_combination(int n, int k);

int n_permutation(IntegerVector group);

int factorial(int n);

#endif
