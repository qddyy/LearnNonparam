#include <algorithm>

#include <Rcpp.h>
#include <cli/progress.h>

#include "rand_wrapper.h"

int n_permutation(Rcpp::IntegerVector group)
{
    double A = 1;

    int n_i = 0;
    int n = group.length();
    int current = group[0];
    for (int i = 0; i < n; i++) {
        A *= (i + 1);
        if (group[i] == current) {
            n_i++;
            A /= n_i;
        } else {
            n_i = 1;
        }
        current = group[i];
    }

    return (int)A;
}

// [[Rcpp::export]]
Rcpp::NumericVector ksample_pmt(
    Rcpp::NumericVector data,
    Rcpp::IntegerVector group,
    Rcpp::Function statistic_func,
    int n_sample)
{
    int total;
    if (n_sample == 0) {
        total = n_permutation(group);
    } else {
        total = n_sample;
    }

    Rcpp::NumericVector statistic_permu(total);
    Rcpp::RObject bar = cli_progress_bar(total, NULL);

    if (n_sample == 0) {
        int i = 0;
        do {
            statistic_permu[i] = Rcpp::as<double>(statistic_func(data, group));

            if (CLI_SHOULD_TICK) {
                cli_progress_set(bar, i);
            }

            i++;
        } while (std::next_permutation(group.begin(), group.end()));
    } else {
        for (int i = 0; i < total; i++) {
            std::random_shuffle(group.begin(), group.end(), rand_wrapper);

            statistic_permu[i] = Rcpp::as<double>(statistic_func(data, group));

            if (CLI_SHOULD_TICK) {
                cli_progress_set(bar, i);
            }
        }
    }

    cli_progress_done(bar);

    return statistic_permu;
}
