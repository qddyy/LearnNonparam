#include <algorithm>

#include <Rcpp.h>
#include <cli/progress.h>

#include "rand_wrapper.h"

int n_combination(int n, int k)
{
    double C = 1;

    for (int i = 1; i <= k; i++) {
        C *= (i + n - k);
        C /= i;
    }

    return (int)C;
}

// [[Rcpp::export]]
Rcpp::NumericVector twosample_pmt(
    int n_1, int n_2,
    Rcpp::NumericVector c_xy,
    Rcpp::Function statistic_func,
    int n_sample)
{
    int total;
    if (n_sample == 0) {
        total = n_combination(n_1 + n_2, std::min(n_1, n_2));
    } else {
        total = n_sample;
    }

    Rcpp::NumericVector statistic_permu(total);
    Rcpp::RObject bar = cli_progress_bar(total, NULL);

    Rcpp::LogicalVector where_x(n_1 + n_2);
    std::fill_n(where_x.begin(), n_1, TRUE);

    if (n_sample == 0) {
        int i = 0;
        do {
            statistic_permu[i] = Rcpp::as<double>(statistic_func(c_xy[where_x], c_xy[!where_x]));

            if (CLI_SHOULD_TICK) {
                cli_progress_set(bar, i);
            }

            i++;
        } while (std::prev_permutation(where_x.begin(), where_x.end()));
    } else {
        for (int i = 0; i < total; i++) {
            std::random_shuffle(c_xy.begin(), c_xy.end(), rand_wrapper);

            statistic_permu[i] = Rcpp::as<double>(statistic_func(c_xy[where_x], c_xy[!where_x]));

            if (CLI_SHOULD_TICK) {
                cli_progress_set(bar, i);
            }
        }
    }

    cli_progress_done(bar);

    return statistic_permu;
}
