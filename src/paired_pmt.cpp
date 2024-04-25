#include "utils.hpp"

template <typename T, typename U, typename V>
NumericVector paired_pmt_impl(
    NumericVector x,
    NumericVector y,
    const U& statistic_func,
    const R_xlen_t n_permu)
{
    T bar;

    V statistic_closure = statistic_func(x, y);
    auto paired_update = [&]() -> bool {
        return bar << statistic_closure(x, y);
    };

    R_len_t i = 0;
    R_len_t n = x.size();
    if (n_permu == 0) {
        bar.init(1 << n, paired_update);

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
        bar.init(n_permu, paired_update);

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
    GENERATE_PMT_BODY(paired, x, y)
}