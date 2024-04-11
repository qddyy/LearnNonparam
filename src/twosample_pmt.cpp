#include "utils.h"

template <typename T>
NumericVector twosample_pmt_impl(
    const NumericVector& data,
    LogicalVector where_y,
    const Function& statistic_func,
    const R_xlen_t n_permu)
{
    T bar;

    auto twosample_update = [&]() -> bool {
        return bar.update(as<double>(statistic_func(data[!where_y], data[where_y])));
    };

    if (n_permu == 0) {
        bar.init(n_permutation(where_y), true);

        do {
            twosample_update();
        } while (next_permutation(where_y));
    } else {
        bar.init(n_permu, false);

        do {
            random_shuffle(where_y);
        } while (twosample_update());
    }

    return bar.close();
}

// [[Rcpp::export]]
NumericVector twosample_pmt(
    const SEXP data,
    const SEXP where_y,
    const SEXP statistic_func,
    const R_xlen_t n_permu,
    const bool progress)
{
    GENERATE_PMT_BODY(twosample_pmt, data, where_y)
}