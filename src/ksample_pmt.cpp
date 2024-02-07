#include "utils.h"

// [[Rcpp::export]]
NumericVector ksample_pmt(
    const NumericVector data,
    IntegerVector group,
    const Function statistic_func,
    const R_xlen_t n_permu)
{
    auto ksample_update = [&](PermuBar& bar) -> bool {
        return bar.update(as<double>(statistic_func(data, group)));
    };

    if (n_permu == 0) {
        PermuBar bar(n_permutation(group), true);

        do {
            ksample_update(bar);
        } while (std::next_permutation(group.begin(), group.end()));

        return bar.statistic_permu;
    } else {
        PermuBar bar(n_permu, false);

        R_len_t size = group.size();
        do {
            random_shuffle(group, size);
        } while (ksample_update(bar));

        return bar.statistic_permu;
    }
}
