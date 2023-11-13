#include <algorithm>
#include <Rcpp.h>
#include <cli/progress.h>
#include "utils.h"

using namespace Rcpp;

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
            statistic_permu[i] = as<double>(statistic_func(data, group));

            if (CLI_SHOULD_TICK) {
                cli_progress_set(bar, i);
            }

            i++;
        } while (std::next_permutation(group.begin(), group.end()));
    } else {
        for (int i = 0; i < total; i++) {
            std::random_shuffle(group.begin(), group.end(), rand_int);

            statistic_permu[i] = as<double>(statistic_func(data, group));

            if (CLI_SHOULD_TICK) {
                cli_progress_set(bar, i);
            }
        }
    }

    cli_progress_done(bar);

    return statistic_permu;
}
