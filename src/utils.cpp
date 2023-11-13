#include "utils.hpp"

// for twosample_pmt
int n_combination(int n, int k)
{
    double C = 1;

    for (int i = 1; i <= k; i++) {
        C *= (i + n - k);
        C /= i;
    }

    return (int)C;
}

// for ksample_pmt
int n_permutation(IntegerVector group)
{
    double A = 1;

    int n_i = 0;
    int n = group.length();
    int current = group[0];
    for (int i = 0; i < n; i++) {
        A *= (i + 1);
        if (group[i] == current) {
            n_i++;
            A /= n_i;
        } else {
            n_i = 1;
        }
        current = group[i];
    }

    return (int)A;
}

// for association_pmt
int factorial(int n)
{
    int A = 1;

    for (int i = 1; i <= n; i++) {
        A *= i;
    }

    return A;
}