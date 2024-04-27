#include "utils.hpp"

template <typename T, typename U, typename V>
NumericVector rcbd_pmt_impl(
    NumericMatrix data,
    const U& statistic_func,
    const R_xlen_t n_permu)
{
    T bar;

    V statistic_closure = statistic_func(data);
    auto rcbd_update = [data, &bar, &statistic_closure]() {
        return bar << statistic_closure(data);
    };

    R_len_t i = 0;
    R_len_t n_col = data.ncol();
    if (n_permu == 0) {
        R_xlen_t total = 1;
        for (R_len_t j = 0; j < n_col; j++) {
            total *= n_permutation(data.column(j));
        }

        bar.init(total, rcbd_update);

        for (R_len_t k = 0; k < n_col; k++) {
            sort(data.column(k));
        }

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
        bar.init(n_permu, rcbd_update);

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
    const NumericMatrix data,
    const RObject statistic_func,
    const R_xlen_t n_permu,
    const bool progress)
{
    GENERATE_PMT_BODY(rcbd, data)
}