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
    int n = y.length();

    int total;
    if (n_permu == 0) {
        total = factorial(n);
    } else {
        total = n_permu;
    }

    NumericVector statistic_permu(total);
    RObject bar = cli_progress_bar(total, NULL);

    NumericVector y_permu(std::move(y));

    if (n_permu == 0) {
        int i = 0;
        do {
            statistic_permu[i] = as<double>(statistic_func(x, y_permu));

            if (CLI_SHOULD_TICK) {
                cli_progress_set(bar, i);
            }

            i++;
        } while (std::next_permutation(y_permu.begin(), y_permu.end()));
    } else {
        for (int i = 0; i < total; i++) {
            std::random_shuffle(y_permu.begin(), y_permu.end(), rand_int);

            statistic_permu[i] = as<double>(statistic_func(x, y_permu));

            if (CLI_SHOULD_TICK) {
                cli_progress_set(bar, i);
            }
        }
    }

    cli_progress_done(bar);

    return statistic_permu;
}
