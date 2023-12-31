#include "utils.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector rcbd_pmt(
    NumericMatrix data,
    const Function statistic_func,
    const R_xlen_t n_permu)
{
    auto rcbd_statistic = [&]() -> double {
        return as<double>(statistic_func(data));
    };

    R_len_t i = 0;
    R_len_t n_col = data.ncol();
    if (n_permu == 0) {
        R_xlen_t total = 1;
        for (R_len_t j = 0; j < n_col; j++) {
            total *= n_permutation(data.column(j));
        }

        PermuBar bar(total, true);

        while (i < n_col) {
            if (i == 0) {
                bar.update(rcbd_statistic());
            }

            if (std::next_permutation(data.column(i).begin(), data.column(i).end())) {
                i = 0;
            } else {
                i++;
            }
        }

        return bar.statistic_permu;
    } else {
        PermuBar bar(n_permu, false);

        do {
            for (i = 0; i < n_col; i++) {
                random_shuffle(data.column(i));
            }
        } while (bar.update(rcbd_statistic()));

        return bar.statistic_permu;
    }
}
