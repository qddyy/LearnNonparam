#include "utils.h"

// [[Rcpp::export]]
NumericVector twosample_pmt(
    const NumericVector data,
    LogicalVector where_y,
    const Function statistic_func,
    const R_xlen_t n_permu)
{
    auto twosample_update = [&](PermuBar& bar) -> bool {
        return bar.update(as<double>(statistic_func(data[!where_y], data[where_y])));
    };

    if (n_permu == 0) {
        PermuBar bar(n_permutation(where_y), true);

        do {
            twosample_update(bar);
        } while (std::next_permutation(where_y.begin(), where_y.end()));

        return bar.statistic_permu;
    } else {
        PermuBar bar(n_permu, false);

        R_len_t size = where_y.size();
        do {
            random_shuffle(where_y, size);
        } while (twosample_update(bar));

        return bar.statistic_permu;
    }
}
