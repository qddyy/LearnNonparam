#include "utils.h"

using namespace Rcpp;

inline void association_do(
    unsigned& i,
    const NumericVector& x,
    const NumericVector& y,
    const Function& statistic_func,
    NumericVector& statistic_permu,
    ProgressBar& bar)
{
    statistic_permu[i] = as<double>(statistic_func(x, y));

    bar.update(i);
    i++;
}

// [[Rcpp::export]]
NumericVector association_pmt(
    const NumericVector x,
    NumericVector y,
    const Function statistic_func,
    const unsigned n_permu)
{
    ProgressBar bar;
    NumericVector statistic_permu;

    unsigned i = 0;
    if (n_permu == 0) {
        std::tie(statistic_permu, bar) = statistic_permu_with_bar(n_permutation(y), true);

        do {
            association_do(i, x, y, statistic_func, statistic_permu, bar);
        } while (std::next_permutation(y.begin(), y.end()));
    } else {
        std::tie(statistic_permu, bar) = statistic_permu_with_bar(n_permu, false);

        while (i < n_permu) {
            random_shuffle(y);
            association_do(i, x, y, statistic_func, statistic_permu, bar);
        }
    }

    bar.done();

    return statistic_permu;
}
