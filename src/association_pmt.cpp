#include "utils.h"

template <typename T>
NumericVector association_pmt_impl(
    const NumericVector& x,
    NumericVector y,
    const Function& statistic_func,
    const R_xlen_t n_permu)
{
    T bar;

    auto association_update = [&]() -> bool {
        return bar.update(as<double>(statistic_func(x, y)));
    };

    if (n_permu == 0) {
        bar.init(n_permutation(y), true);

        do {
            association_update();
        } while (next_permutation(y));
    } else {
        bar.init(n_permu, false);

        do {
            random_shuffle(y);
        } while (association_update());
    }

    return bar.close();
}

// [[Rcpp::export]]
NumericVector association_pmt(
    const SEXP x,
    const SEXP y,
    const SEXP statistic_func,
    const R_xlen_t n_permu,
    const bool progress)
{
    GENERATE_PMT_BODY(association_pmt, x, y)
}