#include "utils.h"

template <typename T>
NumericVector multicomp_pmt_impl(
    const IntegerVector& group_i,
    const IntegerVector& group_j,
    const NumericVector& data,
    IntegerVector group,
    const Function& statistic_func,
    const R_xlen_t n_permu)
{
    T bar;

    R_len_t n_group = group[group.size() - 1];
    R_len_t n_pair = n_group * (n_group - 1) / 2;

    auto multicomp_update = [&]() -> bool {
        Function statistic_func_ij = statistic_func(data, group);

        R_len_t k;
        for (k = 0; k < n_pair - 1; k++) {
            bar.update(as<double>(statistic_func_ij(group_i[k], group_j[k])));
        };

        return bar.update(as<double>(statistic_func_ij(group_i[k], group_j[k])));
    };

    if (n_permu == 0) {
        bar.init(n_permutation(group), true, n_pair);

        do {
            multicomp_update();
        } while (next_permutation(group));
    } else {
        bar.init(n_permu, false, n_pair);

        do {
            random_shuffle(group);
        } while (multicomp_update());
    }

    return bar.close();
}

// [[Rcpp::export]]
NumericVector multicomp_pmt(
    const SEXP group_i,
    const SEXP group_j,
    const SEXP data,
    const SEXP group,
    const SEXP statistic_func,
    const R_xlen_t n_permu,
    const bool progress)
{
    GENERATE_PMT_BODY(multicomp_pmt, group_i, group_j, data, group)
}