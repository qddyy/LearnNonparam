#include "utils.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector paired_pmt(
    const unsigned n,
    const Function statistic_func,
    const unsigned n_permu)
{
    LogicalVector swapped(n);

    unsigned i = 0;

    auto paired_statistic = [&]() -> double {
        for (unsigned j = 0; j < n; j++) {
            swapped[j] = ((i & (1 << j)) != 0);
        }

        return as<double>(statistic_func(swapped));
    };

    unsigned total = (1 << n);

    if (n_permu == 0) {
        PermuBar bar(total, true);

        do {
            bar.update(paired_statistic());
            i++;
        } while (i < total);

        return bar.statistic_permu;
    } else {
        PermuBar bar(n_permu, false);

        do {
            i = rand_int(total);
        } while (bar.update(paired_statistic()));

        return bar.statistic_permu;
    }
}