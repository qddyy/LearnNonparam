#include "utils.h"
#include <Rcpp.h>
#include <algorithm>
#include <cli/progress.h>

using namespace Rcpp;

int n_combination(int n, int k)
{
    double C = 1;

    for (int i = 1; i <= k; i++) {
        C *= (i + n - k);
        C /= i;
    }

    return (int)C;
}

inline void twosample_do(
    int i,
    NumericVector c_xy,
    Function statistic_func,
    NumericVector statistic_permu,
    LogicalVector where_x, RObject bar)
{
    statistic_permu[i] = as<double>(statistic_func(c_xy[where_x], c_xy[!where_x]));

    if (CLI_SHOULD_TICK) {
        cli_progress_set(bar, i);
    }
}

// [[Rcpp::export]]
NumericVector twosample_pmt(
    int n_1, int n_2,
    NumericVector c_xy,
    Function statistic_func,
    int n_permu)
{
    int total;
    if (n_permu == 0) {
        total = n_combination(n_1 + n_2, std::min(n_1, n_2));
    } else {
        total = n_permu;
    }

    NumericVector statistic_permu(total);
    RObject bar = cli_progress_bar(total, NULL);

    LogicalVector where_x(n_1 + n_2, FALSE);
    for (int k = 0; k < n_1; k++) {
        where_x[k] = TRUE;
    }

    if (n_permu == 0) {
        int i = 0;
        do {
            twosample_do(i, c_xy, statistic_func, statistic_permu, where_x, bar);
            i++;
        } while (std::prev_permutation(where_x.begin(), where_x.end()));
    } else {
        for (int i = 0; i < total; i++) {
            random_shuffle(c_xy);
            twosample_do(i, c_xy, statistic_func, statistic_permu, where_x, bar);
        }
    }

    cli_progress_done(bar);

    return statistic_permu;
}
