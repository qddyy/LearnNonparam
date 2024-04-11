#include "utils.h"

template <typename T>
NumericVector paired_pmt_impl(
    NumericVector x,
    NumericVector y,
    const Function& statistic_func,
    const R_xlen_t n_permu)
{
    T bar;

    auto paired_update = [&]() -> bool {
        return bar.update(as<double>(statistic_func(x, y)));
    };

    R_len_t i = 0;
    R_len_t n = x.size();
    if (n_permu == 0) {
        bar.init(1 << n, true);

        IntegerVector swapped(n, 0);
        while (i < n) {
            if (i == 0) {
                paired_update();
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
    } else {
        bar.init(n_permu, false);

        do {
            for (i = 0; i < n; i++) {
                if (rand_int(2) == 1) {
                    std::swap(x[i], y[i]);
                }
            }
        } while (paired_update());
    }

    return bar.close();
}

// [[Rcpp::export]]
NumericVector paired_pmt(
    const SEXP x,
    const SEXP y,
    const SEXP statistic_func,
    const R_xlen_t n_permu,
    const bool progress)
{
    GENERATE_PMT_BODY(paired_pmt, x, y)
}