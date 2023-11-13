#include <algorithm>
#include <Rcpp.h>
#include <cli/progress.h>
#include "utils.hpp"

using namespace Rcpp;

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

    LogicalVector where_x(n_1 + n_2);
    std::fill_n(where_x.begin(), n_1, TRUE);

    if (n_permu == 0) {
        int i = 0;
        do {
            statistic_permu[i] = as<double>(statistic_func(c_xy[where_x], c_xy[!where_x]));

            if (CLI_SHOULD_TICK) {
                cli_progress_set(bar, i);
            }

            i++;
        } while (std::prev_permutation(where_x.begin(), where_x.end()));
    } else {
        for (int i = 0; i < total; i++) {
            std::random_shuffle(c_xy.begin(), c_xy.end(), rand_int);

            statistic_permu[i] = as<double>(statistic_func(c_xy[where_x], c_xy[!where_x]));

            if (CLI_SHOULD_TICK) {
                cli_progress_set(bar, i);
            }
        }
    }

    cli_progress_done(bar);

    return statistic_permu;
}
