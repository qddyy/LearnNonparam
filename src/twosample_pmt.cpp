#include "utils.h"

using namespace Rcpp;

inline void twosample_do(
    unsigned& i,
    const NumericVector& data,
    const LogicalVector& where_y,
    const Function& statistic_func,
    NumericVector& statistic_permu,
    ProgressBar& bar)
{
    statistic_permu[i] = as<double>(statistic_func(data[!where_y], data[where_y]));

    bar.update(i);
    i++;
}

// [[Rcpp::export]]
NumericVector twosample_pmt(
    const NumericVector data,
    LogicalVector where_y,
    const Function statistic_func,
    const unsigned n_permu)
{
    ProgressBar bar;
    NumericVector statistic_permu;

    unsigned i = 0;
    if (n_permu == 0) {
        std::tie(statistic_permu, bar) = statistic_permu_with_bar(n_permutation(where_y), true);

        do {
            twosample_do(i, data, where_y, statistic_func, statistic_permu, bar);
        } while (std::next_permutation(where_y.begin(), where_y.end()));
    } else {
        std::tie(statistic_permu, bar) = statistic_permu_with_bar(n_permu, false);

        while (i < n_permu) {
            random_shuffle(where_y);
            twosample_do(i, data, where_y, statistic_func, statistic_permu, bar);
        }
    }

    bar.done();

    return statistic_permu;
}
