#include "progress.h"
#include "utils.h"

template <typename T>
NumericVector association_pmt_impl(
    const NumericVector& x,
    NumericVector y,
    const Function& statistic_func,
    const R_xlen_t n_permu)
{
    auto association_update = [&](T& bar) -> bool {
        return bar.update(as<double>(statistic_func(x, y)));
    };

    if (n_permu == 0) {
        T bar(n_permutation(y), true);

        do {
            association_update(bar);
        } while (std::next_permutation(y.begin(), y.end()));

        return bar.statistic_permu;
    } else {
        T bar(n_permu, false);

        do {
            random_shuffle(y);
        } while (association_update(bar));

        return bar.statistic_permu;
    }
}

// [[Rcpp::export]]
NumericVector association_pmt(
    const NumericVector x,
    const NumericVector y,
    const Function statistic_func,
    const R_xlen_t n_permu,
    const bool progress)
{
    if (progress) {
        return association_pmt_impl<PermuBarAppear>(x, y, statistic_func, n_permu);
    } else {
        return association_pmt_impl<PermuBarDisappear>(x, y, statistic_func, n_permu);
    }
}