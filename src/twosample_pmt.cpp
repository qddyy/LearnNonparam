#include "utils.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector twosample_pmt(
    const NumericVector data,
    LogicalVector where_y,
    const Function statistic_func,
    const R_xlen_t n_permu)
{
    auto twosample_statistic = [&]() -> double {
        return as<double>(statistic_func(data[!where_y], data[where_y]));
    };

    if (n_permu == 0) {
        PermuBar bar(n_permutation(where_y), true);

        do {
            bar.update(twosample_statistic());
        } while (std::next_permutation(where_y.begin(), where_y.end()));

        return bar.statistic_permu;
    } else {
        PermuBar bar(n_permu, false);

        do {
            random_shuffle(where_y);
        } while (bar.update(twosample_statistic()));

        return bar.statistic_permu;
    }
}
