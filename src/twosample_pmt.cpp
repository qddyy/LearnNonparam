#include "utils.h"

using namespace Rcpp;

inline bool twosample_update(
    PermuBar& bar,
    const NumericVector& data,
    const LogicalVector& where_y,
    const Function& statistic_func)
{
    return bar.update(as<double>(statistic_func(data[!where_y], data[where_y])));
}

// [[Rcpp::export]]
NumericVector twosample_pmt(
    const NumericVector data,
    LogicalVector where_y,
    const Function statistic_func,
    const unsigned n_permu)
{
    if (n_permu == 0) {
        PermuBar bar(n_permutation(where_y), true);

        do {
            twosample_update(bar, data, where_y, statistic_func);
        } while (std::next_permutation(where_y.begin(), where_y.end()));

        return bar.statistic_permu;
    } else {
        PermuBar bar(n_permu, false);

        do {
            random_shuffle(where_y);
        } while (twosample_update(bar, data, where_y, statistic_func));

        return bar.statistic_permu;
    }
}
