#include <Rcpp.h>
#include <cli/progress.h>
#include "utils.hpp"

using namespace Rcpp;

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
            statistic_permu[i] = as<double>(statistic_func(x, y));

            if (CLI_SHOULD_TICK) {
                cli_progress_set(bar, i);
            }

            i++;
        } while (std::next_permutation(y.begin(), y.end()));
    } else {
        for (int i = 0; i < total; i++) {
            std::random_shuffle(y.begin(), y.end(), rand_int);

            statistic_permu[i] = as<double>(statistic_func(x, y));

            if (CLI_SHOULD_TICK) {
                cli_progress_set(bar, i);
            }
        }
    }

    cli_progress_done(bar);

    return statistic_permu;
}
