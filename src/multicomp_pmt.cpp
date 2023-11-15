#include "utils.h"
#include <Rcpp.h>
#include <algorithm>
#include <cli/progress.h>

using namespace Rcpp;

inline void multicomp_do(
    int i, int n, int n_pair,
    IntegerVector group_i,
    IntegerVector group_j,
    NumericVector data,
    IntegerVector group,
    Function statistic_func,
    NumericMatrix statistic_permu,
    List split, RObject bar)
{
    for (int j = 0; j < n; j++) {
        split[j] = data[group == j];
    }

    for (int k = 0; k < n_pair; k++) {
        statistic_permu(k, i) = as<double>(statistic_func(split[group_i[k]], split[group_j[k]], data, group));
    }

    if (CLI_SHOULD_TICK) {
        cli_progress_set(bar, i);
    }
}

// [[Rcpp::export]]
NumericMatrix multicomp_pmt(
    IntegerVector group_i,
    IntegerVector group_j,
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

    int n_pair = group_i.size();

    NumericMatrix statistic_permu(n_pair, total);
    RObject bar = cli_progress_bar(total, NULL);

    int n = *std::max_element(group.begin(), group.end()) + 1;
    List split(n);

    if (n_permu == 0) {
        int i = 0;
        do {
            multicomp_do(i, n, n_pair, group_i, group_j, data, group, statistic_func, statistic_permu, split, bar);
            i++;
        } while (std::next_permutation(group.begin(), group.end()));
    } else {
        for (int i = 0; i < total; i++) {
            random_shuffle(group);
            multicomp_do(i, n, n_pair, group_i, group_j, data, group, statistic_func, statistic_permu, split, bar);
        }
    }

    cli_progress_done(bar);

    return statistic_permu;
}
