#include "progress.h"
#include "utils.h"

template <typename T>
NumericVector ksample_pmt_impl(
    const NumericVector& data,
    IntegerVector group,
    const Function& statistic_func,
    const R_xlen_t n_permu)
{
    auto ksample_update = [&](T& bar) -> bool {
        return bar.update(as<double>(statistic_func(data, group)));
    };

    if (n_permu == 0) {
        T bar(n_permutation(group), true);

        do {
            ksample_update(bar);
        } while (std::next_permutation(group.begin(), group.end()));

        return bar.statistic_permu;
    } else {
        T bar(n_permu, false);

        do {
            random_shuffle(group);
        } while (ksample_update(bar));

        return bar.statistic_permu;
    }
}

// [[Rcpp::export]]
NumericVector ksample_pmt(
    const NumericVector data,
    const IntegerVector group,
    const Function statistic_func,
    const R_xlen_t n_permu,
    const bool progress)
{
    if (progress) {
        return ksample_pmt_impl<PermuBarAppear>(data, group, statistic_func, n_permu);
    } else {
        return ksample_pmt_impl<PermuBarDisappear>(data, group, statistic_func, n_permu);
    }
}