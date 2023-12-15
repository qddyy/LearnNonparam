#include "utils.h"

using namespace Rcpp;

inline bool multicomp_update(
    PermuBar& bar,
    const unsigned& n_pair,
    const unsigned& n_group,
    const IntegerVector& group_i,
    const IntegerVector& group_j,
    const NumericVector& data,
    const IntegerVector& group,
    const Function& statistic_func,
    List& split)
{
    for (unsigned i = 1; i <= n_group; i++) {
        split[i - 1] = data[group == i];
    }

    unsigned j;
    for (j = 0; j < n_pair - 1; j++) {
        bar.update(as<double>(statistic_func(split[group_i[j]], split[group_j[j]], data, group)));
    }

    return bar.update(as<double>(statistic_func(split[group_i[j]], split[group_j[j]], data, group)));
}

// [[Rcpp::export]]
NumericVector multicomp_pmt(
    const IntegerVector group_i,
    const IntegerVector group_j,
    const NumericVector data,
    IntegerVector group,
    const Function statistic_func,
    const unsigned n_permu)
{
    unsigned n_group = group[group.size() - 1];
    unsigned n_pair = n_group * (n_group - 1) / 2;

    List split(n_group);

    if (n_permu == 0) {
        PermuBar bar(n_permutation(group), true, n_pair);

        do {
            multicomp_update(bar, n_pair, n_group, group_i, group_j, data, group, statistic_func, split);
        } while (std::next_permutation(group.begin(), group.end()));

        return bar.statistic_permu;
    } else {
        PermuBar bar(n_permu, false, n_pair);

        do {
            random_shuffle(group);
        } while (multicomp_update(bar, n_pair, n_group, group_i, group_j, data, group, statistic_func, split));

        return bar.statistic_permu;
    }
}
