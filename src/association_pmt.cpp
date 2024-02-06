#include "utils.h"


// [[Rcpp::export]]
NumericVector association_pmt(
    const NumericVector x,
    NumericVector y,
    const Function statistic_func,
    const R_xlen_t n_permu)
{
    auto association_update = [&](PermuBar& bar) -> bool {
        return bar.update(as<double>(statistic_func(x, y)));
    };

    if (n_permu == 0) {
        PermuBar bar(n_permutation(y), true);

        do {
            association_update(bar);
        } while (std::next_permutation(y.begin(), y.end()));

        return bar.statistic_permu;
    } else {
        PermuBar bar(n_permu, false);

        do {
            random_shuffle(y);
        } while (association_update(bar));

        return bar.statistic_permu;
    }
}
