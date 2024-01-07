#include "utils.h"

using namespace Rcpp;

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

    R_len_t k;

    auto multicomp_statistic = [&]() -> double {
        return as<double>(statistic_func(group_i[k], group_j[k], data, group));
    };

    if (n_permu == 0) {
        PermuBar bar(n_permutation(group), true, n_pair);

        do {
            for (k = 0; k < n_pair; k++) {
                bar.update(multicomp_statistic());
            };
        } while (std::next_permutation(group.begin(), group.end()));

        return bar.statistic_permu;
    } else {
        PermuBar bar(n_permu, false, n_pair);

        do {
            random_shuffle(group);
            for (k = 0; k < n_pair - 1; k++) {
                bar.update(multicomp_statistic());
            };
        } while (bar.update(multicomp_statistic()));

        return bar.statistic_permu;
    }
}
