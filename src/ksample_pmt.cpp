#include "utils.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector ksample_pmt(
    const NumericVector data,
    IntegerVector group,
    const Function statistic_func,
    const R_xlen_t n_permu)
{
    auto ksample_statistic = [&]() -> double {
        return as<double>(statistic_func(data, group));
    };

    if (n_permu == 0) {
        PermuBar bar(n_permutation(group), true);

        do {
            bar.update(ksample_statistic());
        } while (std::next_permutation(group.begin(), group.end()));

        return bar.statistic_permu;
    } else {
        PermuBar bar(n_permu, false);

        do {
            random_shuffle(group);
        } while (bar.update(ksample_statistic()));

        return bar.statistic_permu;
    }
}
