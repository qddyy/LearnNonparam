#include "progress.h"
#include "utils.h"

template <typename T>
NumericVector twosample_pmt_impl(
    const NumericVector& data,
    LogicalVector where_y,
    const Function& statistic_func,
    const R_xlen_t n_permu)
{
    auto twosample_update = [&](T& bar) -> bool {
        return bar.update(as<double>(statistic_func(data[!where_y], data[where_y])));
    };

    if (n_permu == 0) {
        T bar(n_permutation(where_y), true);

        do {
            twosample_update(bar);
        } while (std::next_permutation(where_y.begin(), where_y.end()));

        return bar.statistic_permu;
    } else {
        T bar(n_permu, false);

        do {
            random_shuffle(where_y);
        } while (twosample_update(bar));

        return bar.statistic_permu;
    }
}

// [[Rcpp::export]]
NumericVector twosample_pmt(
    const NumericVector data,
    const LogicalVector where_y,
    const Function statistic_func,
    const R_xlen_t n_permu,
    const bool progress)
{
    if (progress) {
        return twosample_pmt_impl<PermuBarAppear>(data, where_y, statistic_func, n_permu);
    } else {
        return twosample_pmt_impl<PermuBarDisappear>(data, where_y, statistic_func, n_permu);
    }
}