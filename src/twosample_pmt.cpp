#include "utils.hpp"

template <typename T, typename U, typename V>
NumericVector twosample_pmt_impl(
    NumericVector x,
    NumericVector y,
    const U& statistic_func,
    const R_xlen_t n_permu)
{
    T bar;

    R_len_t m = x.size();
    R_len_t n = m + y.size();

    NumericVector data(no_init(n));
    std::copy(x.begin(), x.end(), data.begin());
    std::copy(y.begin(), y.end(), data.begin() + m);

    LogicalVector where_y(no_init(n));
    std::fill(where_y.begin(), where_y.begin() + m, false);
    std::fill(where_y.begin() + m, where_y.end(), true);

    V statistic_closure = statistic_func(x, y);
    auto twosample_update = [x, y, n, data, where_y, &bar, &statistic_closure]() mutable {
        R_len_t i = 0;
        R_len_t j = 0;
        for (R_len_t k = 0; k < n; k++) {
            if (where_y[k]) {
                y[i++] = data[k];
            } else {
                x[j++] = data[k];
            }
        }

        return bar << statistic_closure(x, y);
    };

    if (n_permu == 0) {
        bar.init(n_permutation(where_y), twosample_update);

        do {
            twosample_update();
        } while (next_permutation(where_y));
    } else {
        bar.init(n_permu, twosample_update);

        do {
            random_shuffle(where_y);
        } while (twosample_update());
    }

    return bar.close();
}

// [[Rcpp::export]]
NumericVector twosample_pmt(
    const NumericVector x,
    const NumericVector y,
    const RObject statistic_func,
    const R_xlen_t n_permu,
    const bool progress)
{
    GENERATE_PMT_BODY(twosample, x, y)
}