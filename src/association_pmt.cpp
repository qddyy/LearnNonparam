#include "utils.h"

using namespace Rcpp;

inline bool association_update(
    PermuBar& bar,
    const NumericVector& x,
    const NumericVector& y,
    const Function& statistic_func)
{
    return bar.update(as<double>(statistic_func(x, y)));
}

// [[Rcpp::export]]
NumericVector association_pmt(
    const NumericVector x,
    NumericVector y,
    const Function statistic_func,
    const unsigned n_permu)
{
    if (n_permu == 0) {
        PermuBar bar(n_permutation(y), true);

        do {
            association_update(bar, x, y, statistic_func);
        } while (std::next_permutation(y.begin(), y.end()));

        return bar.statistic_permu;
    } else {
        PermuBar bar(n_permu, false);

        do {
            random_shuffle(y);
        } while (association_update(bar, x, y, statistic_func));

        return bar.statistic_permu;
    }
}
