#include "utils.h"

using namespace Rcpp;

inline bool paired_update(
    PermuBar& bar,
    const unsigned& i,
    const unsigned& n,
    const Function& statistic_func,
    LogicalVector& swapped)
{
    for (unsigned j = 0; j < n; j++) {
        swapped[j] = ((i & (1 << j)) != 0);
    }

    return bar.update(as<double>(statistic_func(swapped)));
}

// [[Rcpp::export]]
NumericVector paired_pmt(
    const unsigned n,
    const Function statistic_func,
    const unsigned n_permu)
{
    unsigned total = (1 << n);

    LogicalVector swapped(n);

    unsigned i = 0;
    if (n_permu == 0) {
        PermuBar bar(total, true);

        do {
            paired_update(bar, i, n, statistic_func, swapped);
            i++;
        } while (i < total);

        return bar.statistic_permu;
    } else {
        PermuBar bar(n_permu, false);

        do {
            i = rand_int(total);
        } while (paired_update(bar, i, n, statistic_func, swapped));

        return bar.statistic_permu;
    }
}