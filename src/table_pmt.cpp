#include "utils.h"
#include <Rcpp.h>
#include <algorithm>
#include <cli/progress.h>

using namespace Rcpp;

inline void table_do(
    int i, int n,
    IntegerVector row_loc,
    IntegerVector col_loc,
    Function statistic_func,
    NumericVector statistic_permu,
    IntegerMatrix data, RObject bar)
{
    std::fill(data.begin(), data.end(), 0);

    for (int j = 0; j < n; j++) {
        data(row_loc[j], col_loc[j])++;
    }

    statistic_permu[i] = as<double>(statistic_func(data));

    if (CLI_SHOULD_TICK) {
        cli_progress_set(bar, i);
    }
}

// [[Rcpp::export]]
NumericVector table_pmt(
    IntegerVector row_loc,
    IntegerVector col_loc,
    Function statistic_func,
    int n_permu)
{
    int total;
    if (n_permu == 0) {
        total = n_permutation(row_loc);
    } else {
        total = n_permu;
    }

    NumericVector statistic_permu(total);
    RObject bar = cli_progress_bar(total, NULL);

    int n = row_loc.size();
    IntegerMatrix data(row_loc[n - 1] + 1, col_loc[n - 1] + 1);

    if (n_permu == 0) {
        int i = 0;
        do {
            table_do(i, n, row_loc, col_loc, statistic_func, statistic_permu, data, bar);
            i++;
        } while (std::next_permutation(row_loc.begin(), row_loc.end()));
    } else {
        for (int i = 0; i < total; i++) {
            random_shuffle(row_loc);
            table_do(i, n, row_loc, col_loc, statistic_func, statistic_permu, data, bar);
        }
    }

    cli_progress_done(bar);

    return statistic_permu;
}
