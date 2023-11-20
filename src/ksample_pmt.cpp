#include "utils.h"

using namespace Rcpp;

inline void ksample_do(
    unsigned& i,
    const NumericVector& data,
    const IntegerVector& group,
    const Function& statistic_func,
    NumericVector& statistic_permu,
    RObject& bar)
{
    statistic_permu[i] = as<double>(statistic_func(data, group));

    if (CLI_SHOULD_TICK) {
        cli_progress_set(bar, i);
    }
    i++;
}

// [[Rcpp::export]]
NumericVector ksample_pmt(
    const NumericVector data,
    IntegerVector group,
    const Function statistic_func,
    const unsigned n_permu)
{
    RObject bar;
    cli_progress_init_timer();
    NumericVector statistic_permu;

    unsigned i = 0;
    if (n_permu == 0) {
        std::tie(statistic_permu, bar) = statistic_permu_with_bar(n_permutation(group), true);

        do {
            ksample_do(i, data, group, statistic_func, statistic_permu, bar);
        } while (std::next_permutation(group.begin(), group.end()));
    } else {
        std::tie(statistic_permu, bar) = statistic_permu_with_bar(n_permu, false);

        while (i < n_permu) {
            random_shuffle(group);
            ksample_do(i, data, group, statistic_func, statistic_permu, bar);
        }
    }

    cli_progress_done(bar);

    return statistic_permu;
}
