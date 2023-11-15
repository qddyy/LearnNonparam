#include "utils.hpp"
#include <Rcpp.h>
#include <cli/progress.h>

using namespace Rcpp;

inline void paired_do(
    int i,
    Function statistic_func,
    NumericVector statistic_permu,
    LogicalVector swapped, RObject bar)
{
    statistic_permu[i] = as<double>(statistic_func(swapped));

    if (CLI_SHOULD_TICK) {
        cli_progress_set(bar, i);
    }
}

// [[Rcpp::export]]
NumericVector paired_pmt(
    int n,
    Function statistic_func,
    int n_permu)
{
    int total;
    if (n_permu == 0) {
        total = (1 << n);
    } else {
        total = n_permu;
    }

    NumericVector statistic_permu(total);
    RObject bar = cli_progress_bar(total, NULL);

    LogicalVector swapped(n);

    if (n_permu == 0) {
        for (int i = 0; i < total; i++) {
            for (int j = 0; j < n; j++) {
                swapped[j] = ((i & (1 << j)) != 0);
            }
            paired_do(i, statistic_func, statistic_permu, swapped, bar);
        }
    } else {
        int r_int;
        for (int i = 0; i < total; i++) {
            r_int = rand_int(total);
            for (int j = 0; j < n; j++) {
                swapped[j] = ((r_int & (1 << j)) != 0);
            }
            paired_do(i, statistic_func, statistic_permu, swapped, bar);
        }
    }

    cli_progress_done(bar);

    return statistic_permu;
}