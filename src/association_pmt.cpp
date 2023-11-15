#include "utils.hpp"
#include <Rcpp.h>
#include <algorithm>
#include <cli/progress.h>

using namespace Rcpp;

inline void association_do(
    int i,
    NumericVector x,
    NumericVector y,
    Function statistic_func,
    NumericVector statistic_permu,
    RObject bar)
{
    statistic_permu[i] = as<double>(statistic_func(x, y));

    if (CLI_SHOULD_TICK) {
        cli_progress_set(bar, i);
    }
}

// [[Rcpp::export]]
NumericVector association_pmt(
    NumericVector x,
    NumericVector y,
    Function statistic_func,
    int n_permu)
{
    int total;
    if (n_permu == 0) {
        total = n_permutation(y);
    } else {
        total = n_permu;
    }

    NumericVector statistic_permu(total);
    RObject bar = cli_progress_bar(total, NULL);

    if (n_permu == 0) {
        int i = 0;
        do {
            association_do(i, x, y, statistic_func, statistic_permu, bar);
            i++;
        } while (std::next_permutation(y.begin(), y.end()));
    } else {
        for (int i = 0; i < total; i++) {
            std::random_shuffle(y.begin(), y.end(), rand_int);
            association_do(i, x, y, statistic_func, statistic_permu, bar);
        }
    }

    cli_progress_done(bar);

    return statistic_permu;
}
