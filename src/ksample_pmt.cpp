#include "utils.h"

using namespace Rcpp;

inline bool ksample_update(
    PermuBar& bar,
    const NumericVector& data,
    const IntegerVector& group,
    const Function& statistic_func)
{
    return bar.update(as<double>(statistic_func(data, group)));
}

// [[Rcpp::export]]
NumericVector ksample_pmt(
    const NumericVector data,
    IntegerVector group,
    const Function statistic_func,
    const unsigned n_permu)
{
    if (n_permu == 0) {
        PermuBar bar(n_permutation(group), true);

        do {
            ksample_update(bar, data, group, statistic_func);
        } while (std::next_permutation(group.begin(), group.end()));

        return bar.statistic_permu;
    } else {
        PermuBar bar(n_permu, false);

        do {
            random_shuffle(group);
        } while (ksample_update(bar, data, group, statistic_func));

        return bar.statistic_permu;
    }
}
