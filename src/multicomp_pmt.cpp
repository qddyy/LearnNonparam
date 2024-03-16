#include "progress.h"
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
    R_len_t n_group = group[group.size() - 1];
    R_len_t n_pair = n_group * (n_group - 1) / 2;

    auto multicomp_update = [&](T& bar) -> bool {
        Function statistic_func_ij = statistic_func(data, group);

        R_len_t k;
        for (k = 0; k < n_pair - 1; k++) {
            bar.update(as<double>(statistic_func_ij(group_i[k], group_j[k])));
        };

        return bar.update(as<double>(statistic_func_ij(group_i[k], group_j[k])));
    };

    if (n_permu == 0) {
        T bar(n_permutation(group), true, n_pair);

        do {
            multicomp_update(bar);
        } while (std::next_permutation(group.begin(), group.end()));

        return bar.statistic_permu;
    } else {
        T bar(n_permu, false, n_pair);

        do {
            random_shuffle(group);
        } while (multicomp_update(bar));

        return bar.statistic_permu;
    }
}

// [[Rcpp::export]]
NumericVector multicomp_pmt(
    const IntegerVector group_i,
    const IntegerVector group_j,
    const NumericVector data,
    const IntegerVector group,
    const Function statistic_func,
    const R_xlen_t n_permu,
    const bool progress)
{
    if (progress) {
        return multicomp_pmt_impl<PermuBarAppear>(group_i, group_j, data, group, statistic_func, n_permu);
    } else {
        return multicomp_pmt_impl<PermuBarDisappear>(group_i, group_j, data, group, statistic_func, n_permu);
    }
}