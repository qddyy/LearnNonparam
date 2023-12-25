#include "utils.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector association_pmt(
    const NumericVector x,
    NumericVector y,
    const Function statistic_func,
    const R_xlen_t n_permu)
{
    auto association_statistic = [&]() -> double {
        return as<double>(statistic_func(x, y));
    };

    if (n_permu == 0) {
        PermuBar bar(n_permutation(y), true);

        do {
            bar.update(association_statistic());
        } while (std::next_permutation(y.begin(), y.end()));

        return bar.statistic_permu;
    } else {
        PermuBar bar(n_permu, false);

        do {
            random_shuffle(y);
        } while (bar.update(association_statistic()));

        return bar.statistic_permu;
    }
}
