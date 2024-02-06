#include "utils.h"

// [[Rcpp::export]]
NumericVector multicomp_pmt(
    const IntegerVector group_i,
    const IntegerVector group_j,
    const NumericVector data,
    IntegerVector group,
    const Function statistic_func,
    const R_xlen_t n_permu)
{
    R_len_t n_group = group[group.size() - 1];
    R_len_t n_pair = n_group * (n_group - 1) / 2;

    auto multicomp_update = [&](PermuBar& bar) -> bool {
        Function statistic_func_ij = statistic_func(data, group);

        R_len_t k;
        for (k = 0; k < n_pair - 1; k++) {
            bar.update(as<double>(statistic_func_ij(group_i[k], group_j[k])));
        };

        return bar.update(as<double>(statistic_func_ij(group_i[k], group_j[k])));
    };

    if (n_permu == 0) {
        PermuBar bar(n_permutation(group), true, n_pair);

        do {
            multicomp_update(bar);
        } while (std::next_permutation(group.begin(), group.end()));

        return bar.statistic_permu;
    } else {
        PermuBar bar(n_permu, false, n_pair);

        do {
            random_shuffle(group);
        } while (multicomp_update(bar));

        return bar.statistic_permu;
    }
}
