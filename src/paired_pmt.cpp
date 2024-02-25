#include "utils.h"

// [[Rcpp::export]]
NumericVector paired_pmt(
    NumericVector x,
    NumericVector y,
    const Function statistic_func,
    const R_xlen_t n_permu)
{
    auto paired_update = [&](PermuBar& bar) -> bool {
        return bar.update(as<double>(statistic_func(x, y)));
    };

    R_len_t i = 0;
    R_len_t n = x.size();
    if (n_permu == 0) {
        PermuBar bar(1 << n, true);

        IntegerVector swapped(n, 0);
        while (i < n) {
            if (i == 0) {
                paired_update(bar);
            }

            std::swap(x[i], y[i]);
            swapped[i]++;

            if (swapped[i] < 2) {
                i = 0;
            } else {
                swapped[i] = 0;
                i++;
            }
        }

        return bar.statistic_permu;
    } else {
        PermuBar bar(n_permu, false);

        do {
            for (i = 0; i < n; i++) {
                if (rand_int(2) == 1) {
                    std::swap(x[i], y[i]);
                }
            }
        } while (paired_update(bar));

        return bar.statistic_permu;
    }
}