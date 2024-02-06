#include "utils.h"

// [[Rcpp::export]]
NumericVector paired_pmt(
    const R_len_t n,
    const Function statistic_func,
    const R_xlen_t n_permu)
{
    LogicalVector swapped(n);

    R_xlen_t i = 0;
    R_xlen_t total = (1 << n);

    auto paired_update = [&](PermuBar& bar) -> bool {
        for (R_len_t j = 0; j < n; j++) {
            swapped[j] = ((i & (1 << j)) != 0);
        }

        return bar.update(as<double>(statistic_func(swapped)));
    };

    if (n_permu == 0) {
        PermuBar bar(total, true);

        do {
            paired_update(bar);
            i++;
        } while (i < total);

        return bar.statistic_permu;
    } else {
        PermuBar bar(n_permu, false);

        do {
            i = rand_int(total);
        } while (paired_update(bar));

        return bar.statistic_permu;
    }
}