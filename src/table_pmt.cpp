#include "progress.h"
#include "utils.h"

template <typename T>
NumericVector table_pmt_impl(
    IntegerVector row_loc,
    const IntegerVector& col_loc,
    const Function& statistic_func,
    const R_xlen_t n_permu)
{
    R_len_t n = row_loc.size();

    IntegerMatrix data(no_init(row_loc[n - 1] + 1, col_loc[n - 1] + 1));

    auto table_update = [&](T& bar) -> bool {
        data.fill(0);
        for (R_len_t i = 0; i < n; i++) {
            data(row_loc[i], col_loc[i])++;
        }

        return bar.update(as<double>(statistic_func(data)));
    };

    if (n_permu == 0) {
        T bar(n_permutation(row_loc), true);

        do {
            table_update(bar);
        } while (std::next_permutation(row_loc.begin(), row_loc.end()));

        return bar.statistic_permu;
    } else {
        T bar(n_permu, false);

        do {
            random_shuffle(row_loc);
        } while (table_update(bar));

        return bar.statistic_permu;
    }
}

// [[Rcpp::export]]
NumericVector table_pmt(
    const IntegerVector row_loc,
    const IntegerVector col_loc,
    const Function statistic_func,
    const R_xlen_t n_permu,
    const bool progress)
{
    if (progress) {
        return table_pmt_impl<PermuBarAppear>(row_loc, col_loc, statistic_func, n_permu);
    } else {
        return table_pmt_impl<PermuBarDisappear>(row_loc, col_loc, statistic_func, n_permu);
    }
}