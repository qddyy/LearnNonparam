#include "utils.h"
#include <Rcpp.h>
#include <algorithm>
#include <cli/progress.h>

using namespace Rcpp;

inline void rcbd_do(
    int i,
    NumericMatrix data,
    Function statistic_func,
    NumericVector statistic_permu,
    RObject bar)
{
    statistic_permu[i] = as<double>(statistic_func(data));

    if (CLI_SHOULD_TICK) {
        cli_progress_set(bar, i);
    }
}

// [[Rcpp::export]]
NumericVector rcbd_pmt(
    NumericMatrix data,
    Function statistic_func,
    int n_permu)
{
    int n_col = data.ncol();

    int total = 1;
    if (n_permu == 0) {
        for (int k = 0; k < n_col; k++) {
            total *= n_permutation(data.column(k));
        }
    } else {
        total = n_permu;
    }

    NumericVector statistic_permu(total);
    RObject bar = cli_progress_bar(total, NULL);

    if (n_permu == 0) {
        int i = 0;
        int j = 0;
        while (j < n_col) {
            if (j == 0) {
                rcbd_do(i, data, statistic_func, statistic_permu, bar);
                i++;
            }

            if (std::next_permutation(data.column(j).begin(), data.column(j).end())) {
                j = 0;
            } else {
                j++;
            }
        }
    } else {
        for (int i = 0; i < total; i++) {
            for (int j = 0; j < n_col; j++) {
                random_shuffle(data.column(j));
            }
            rcbd_do(i, data, statistic_func, statistic_permu, bar);
        }
    }

    cli_progress_done(bar);

    return statistic_permu;
}
