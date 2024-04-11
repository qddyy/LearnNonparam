#include "utils.h"

template <typename T>
NumericVector ksample_pmt_impl(
    const NumericVector& data,
    IntegerVector group,
    const Function& statistic_func,
    const R_xlen_t n_permu)
{
    T bar;

    auto ksample_update = [&]() -> bool {
        return bar.update(as<double>(statistic_func(data, group)));
    };

    if (n_permu == 0) {
        bar.init(n_permutation(group), true);

        do {
            ksample_update();
        } while (next_permutation(group));
    } else {
        bar.init(n_permu, false);

        do {
            random_shuffle(group);
        } while (ksample_update());
    }

    return bar.close();
}

// [[Rcpp::export]]
NumericVector ksample_pmt(
    const SEXP data,
    const SEXP group,
    const SEXP statistic_func,
    const R_xlen_t n_permu,
    const bool progress)
{
    GENERATE_PMT_BODY(ksample_pmt, data, group)
}