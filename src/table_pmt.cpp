#include "utils.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector table_pmt(
    IntegerVector row_loc,
    const IntegerVector col_loc,
    const Function statistic_func,
    const R_xlen_t n_permu)
{
    R_len_t n = row_loc.size();

    IntegerMatrix data(no_init(row_loc[n - 1] + 1, col_loc[n - 1] + 1));

    auto table_statistic = [&]() -> double {
        data.fill(0);
        for (R_len_t i = 0; i < n; i++) {
            data(row_loc[i], col_loc[i])++;
        }

        return as<double>(statistic_func(data));
    };

    if (n_permu == 0) {
        PermuBar bar(n_permutation(row_loc), true);

        do {
            bar.update(table_statistic());
        } while (std::next_permutation(row_loc.begin(), row_loc.end()));

        return bar.statistic_permu;
    } else {
        PermuBar bar(n_permu, false);

        do {
            random_shuffle(row_loc);
        } while (bar.update(table_statistic()));

        return bar.statistic_permu;
    }
}
