#include "utils.hpp"

template <typename T, typename U, typename V>
NumericVector ksample_pmt_impl(
    const NumericVector data,
    IntegerVector group,
    const U& statistic_func,
    const R_xlen_t n_permu)
{
    T bar;

    V statistic_closure = statistic_func(data, group);
    auto ksample_update = [data, group, &bar, &statistic_closure]() {
        return bar << statistic_closure(data, group);
    };

    if (n_permu == 0) {
        bar.init(n_permutation(group), ksample_update);

        do {
            ksample_update();
        } while (next_permutation(group));
    } else {
        bar.init(n_permu, ksample_update);

        do {
            random_shuffle(group);
        } while (ksample_update());
    }

    return bar.close();
}

// [[Rcpp::export]]
NumericVector ksample_pmt(
    const NumericVector data,
    const IntegerVector group,
    const RObject statistic_func,
    const R_xlen_t n_permu,
    const bool progress)
{
    GENERATE_PMT_BODY(ksample, data, group)
}