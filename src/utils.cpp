#include "utils.hpp"

int n_combination(int n, int k)
{
    double C = 1;

    for (int i = 1; i <= k; i++) {
        C *= (i + n - k);
        C /= i;
    }

    return (int)C;
}
