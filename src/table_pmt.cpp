#include "utils.h"

template <typename T>
NumericVector table_pmt_impl(
    IntegerVector row_loc,
    const IntegerVector& col_loc,
    const Function& statistic_func,
    const R_xlen_t n_permu)
{
    T bar;

    R_len_t n = row_loc.size();

    IntegerMatrix data(no_init(row_loc[n - 1] + 1, col_loc[n - 1] + 1));

    auto table_update = [&]() -> bool {
        data.fill(0);
        for (R_len_t i = 0; i < n; i++) {
            data(row_loc[i], col_loc[i])++;
        }

        return bar.update(as<double>(statistic_func(data)));
    };

    if (n_permu == 0) {
        bar.init(n_permutation(row_loc), true);

        do {
            table_update();
        } while (next_permutation(row_loc));
    } else {
        bar.init(n_permu, false);

        do {
            random_shuffle(row_loc);
        } while (table_update());
    }

    return bar.close();
}

// [[Rcpp::export]]
NumericVector table_pmt(
    const SEXP row_loc,
    const SEXP col_loc,
    const SEXP statistic_func,
    const R_xlen_t n_permu,
    const bool progress)
{
    GENERATE_PMT_BODY(table_pmt, row_loc, col_loc)
}