#include "utils.h"
#include <Rcpp.h>
#include <algorithm>
#include <cli/progress.h>

using namespace Rcpp;

inline void ksample_do(
    int i,
    NumericVector data,
    IntegerVector group,
    Function statistic_func,
    NumericVector statistic_permu,
    RObject bar)
{
    statistic_permu[i] = as<double>(statistic_func(data, group));

    if (CLI_SHOULD_TICK) {
        cli_progress_set(bar, i);
    }
}

// [[Rcpp::export]]
NumericVector ksample_pmt(
    NumericVector data,
    IntegerVector group,
    Function statistic_func,
    int n_permu)
{
    int total;
    if (n_permu == 0) {
        total = n_permutation(group);
    } else {
        total = n_permu;
    }

    NumericVector statistic_permu(total);
    RObject bar = cli_progress_bar(total, NULL);

    if (n_permu == 0) {
        int i = 0;
        do {
            ksample_do(i, data, group, statistic_func, statistic_permu, bar);
            i++;
        } while (std::next_permutation(group.begin(), group.end()));
    } else {
        for (int i = 0; i < total; i++) {
            random_shuffle(group);
            ksample_do(i, data, group, statistic_func, statistic_permu, bar);
        }
    }

    cli_progress_done(bar);

    return statistic_permu;
}
