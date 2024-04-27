#include "utils.hpp"

template <typename T, typename U, typename V>
NumericVector twosample_pmt_impl(
    const NumericVector data,
    LogicalVector where_y,
    const U& statistic_func,
    const R_xlen_t n_permu)
{
    T bar;

    V statistic_closure = statistic_func(data[!where_y], data[where_y]);
    auto twosample_update = [data, where_y, &bar, &statistic_closure]() {
        return bar << statistic_closure(data[!where_y], data[where_y]);
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
    const NumericVector data,
    const LogicalVector where_y,
    const RObject statistic_func,
    const R_xlen_t n_permu,
    const bool progress)
{
    GENERATE_PMT_BODY(twosample, data, where_y)
}