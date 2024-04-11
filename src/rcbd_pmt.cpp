#include "utils.h"

template <typename T>
NumericVector rcbd_pmt_impl(
    NumericMatrix data,
    const Function& statistic_func,
    const R_xlen_t n_permu)
{
    T bar;

    auto rcbd_update = [&]() -> bool {
        return bar.update(as<double>(statistic_func(data)));
    };

    R_len_t i = 0;
    R_len_t n_col = data.ncol();
    if (n_permu == 0) {
        R_xlen_t total = 1;
        for (R_len_t j = 0; j < n_col; j++) {
            total *= n_permutation(data.column(j));
        }

        bar.init(total, true);

        while (i < n_col) {
            if (i == 0) {
                rcbd_update();
            }

            if (next_permutation(data.column(i))) {
                i = 0;
            } else {
                i++;
            }
        }
    } else {
        bar.init(n_permu, false);

        do {
            for (i = 0; i < n_col; i++) {
                random_shuffle(data.column(i));
            }
        } while (rcbd_update());
    }

    return bar.close();
}

// [[Rcpp::export]]
NumericVector rcbd_pmt(
    const SEXP data,
    const SEXP statistic_func,
    const R_xlen_t n_permu,
    const bool progress)
{
    GENERATE_PMT_BODY(rcbd_pmt, data)
}