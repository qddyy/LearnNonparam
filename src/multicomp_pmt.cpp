#include "utils.h"

using namespace Rcpp;

inline void multicomp_do(
    unsigned& i,
    const unsigned& n,
    const unsigned& n_pair,
    const IntegerVector& group_i,
    const IntegerVector& group_j,
    const NumericVector& data,
    const IntegerVector& group,
    const Function& statistic_func,
    NumericVector& statistic_permu,
    ProgressBar& bar, List& split)
{
    for (unsigned j = 1; j <= n; j++) {
        split[j - 1] = data[group == j];
    }

    for (unsigned k = 0; k < n_pair; k++) {
        statistic_permu(k, i) = as<double>(statistic_func(split[group_i[k]], split[group_j[k]], data, group));
    }

    bar.update(i);
    i++;
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
    ProgressBar bar;
    NumericVector statistic_permu;

    unsigned n_pair = group_i.size();
    unsigned n = group[group.size() - 1];
    List split(n);

    unsigned i = 0;
    if (n_permu == 0) {
        std::tie(statistic_permu, bar) = statistic_permu_with_bar(n_permutation(group), true, n_pair);

        do {
            multicomp_do(i, n, n_pair, group_i, group_j, data, group, statistic_func, statistic_permu, bar, split);
        } while (std::next_permutation(group.begin(), group.end()));
    } else {
        std::tie(statistic_permu, bar) = statistic_permu_with_bar(n_permu, false, n_pair);

        while (i < n_permu) {
            random_shuffle(group);
            multicomp_do(i, n, n_pair, group_i, group_j, data, group, statistic_func, statistic_permu, bar, split);
        }
    }

    bar.done();

    return statistic_permu;
}
