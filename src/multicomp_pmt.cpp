#include "utils.hpp"

template <typename T, typename U, typename V>
NumericVector multicomp_pmt_impl(
    const IntegerVector& group_i,
    const IntegerVector& group_j,
    const NumericVector& data,
    IntegerVector group,
    const U& statistic_func,
    const R_xlen_t n_permu)
{
    T bar;

    R_len_t n_group = group[group.size() - 1];
    R_len_t n_pair = n_group * (n_group - 1) / 2;

    auto multicomp_update = [&]() -> bool {
        V statistic_closure = statistic_func(data, group);

        bool flag = false;
        for (R_len_t k = 0; k < n_pair; k++) {
            flag = bar << statistic_closure(group_i[k], group_j[k]);
        };

        return flag;
    };

    if (n_permu == 0) {
        bar.init(n_permutation(group), multicomp_update, n_pair);

        do {
            multicomp_update();
        } while (next_permutation(group));
    } else {
        bar.init(n_permu, multicomp_update, n_pair);

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
    Function statistic(statistic_func);
    PMT_PROGRESS_RETURN(multicomp_pmt_impl, Function, Function, group_i, group_j, data, group)
}