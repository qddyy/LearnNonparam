#include "utils.h"

using namespace Rcpp;

inline void paired_do(
    unsigned& i,
    const unsigned& k,
    const unsigned& n,
    const Function& statistic_func,
    NumericVector& statistic_permu,
    RObject& bar, LogicalVector& swapped)
{
    for (unsigned j = 0; j < n; j++) {
        swapped[j] = ((i & (1 << j)) != 0);
    }

    statistic_permu[i] = as<double>(statistic_func(swapped));

    if (CLI_SHOULD_TICK) {
        cli_progress_set(bar, i);
    }
    i++;
}

// [[Rcpp::export]]
NumericVector paired_pmt(
    const unsigned n,
    const Function statistic_func,
    const unsigned n_permu)
{
    RObject bar;
    cli_progress_init_timer();
    NumericVector statistic_permu;

    LogicalVector swapped(n);
    unsigned total = (1 << n);

    unsigned i = 0;
    if (n_permu == 0) {
        std::tie(statistic_permu, bar) = statistic_permu_with_bar(total, true);

        while (i < total) {
            paired_do(i, i, n, statistic_func, statistic_permu, bar, swapped);
        }
    } else {
        std::tie(statistic_permu, bar) = statistic_permu_with_bar(n_permu, false);

        while (i < n_permu) {
            paired_do(i, rand_int(total), n, statistic_func, statistic_permu, bar, swapped);
        }
    }

    cli_progress_done(bar);

    return statistic_permu;
}