#include "utils.hpp"

template <typename T, typename U, typename V>
NumericVector association_pmt_impl(
    NumericVector x,
    NumericVector y,
    const U& statistic_func,
    const R_xlen_t n_permu)
{
    T bar;

    V statistic_closure = statistic_func(x, y);
    auto association_update = [x, y, &bar, &statistic_closure]() {
        return bar << statistic_closure(x, y);
    };

    if (n_permu == 0) {
        bar.init(n_permutation(y), association_update);

        IntegerVector y_order = seq_along(x) - 1;
        sort(y_order, [&y](int i, int j) { return y[i] < y[j]; });

        x = x[y_order];
        y = y[y_order];

        do {
            association_update();
        } while (next_permutation(y));
    } else {
        bar.init(n_permu, association_update);

        do {
            random_shuffle(y);
        } while (association_update());
    }

    return bar.close();
}

// [[Rcpp::export]]
NumericVector association_pmt(
    const NumericVector x,
    const NumericVector y,
    const RObject statistic_func,
    const R_xlen_t n_permu,
    const bool progress)
{
    GENERATE_PMT_BODY(association, x, y)
}