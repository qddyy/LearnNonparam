#include "utils.h"

using namespace Rcpp;

inline void table_do(
    unsigned& i,
    const unsigned& n,
    const IntegerVector& row_loc,
    const IntegerVector& col_loc,
    const Function& statistic_func,
    NumericVector& statistic_permu,
    ProgressBar& bar, IntegerMatrix& data)
{
    data.fill(0);

    for (unsigned j = 0; j < n; j++) {
        data(row_loc[j], col_loc[j])++;
    }

    statistic_permu[i] = as<double>(statistic_func(data));

    bar.update(i);
    i++;
}

// [[Rcpp::export]]
NumericVector table_pmt(
    IntegerVector row_loc,
    const IntegerVector col_loc,
    const Function statistic_func,
    const unsigned n_permu)
{
    ProgressBar bar;
    NumericVector statistic_permu;

    unsigned n = row_loc.size();
    IntegerMatrix data(row_loc[n - 1] + 1, col_loc[n - 1] + 1);

    unsigned i = 0;
    if (n_permu == 0) {
        std::tie(statistic_permu, bar) = statistic_permu_with_bar(n_permutation(row_loc), true);

        do {
            table_do(i, n, row_loc, col_loc, statistic_func, statistic_permu, bar, data);
        } while (std::next_permutation(row_loc.begin(), row_loc.end()));
    } else {
        std::tie(statistic_permu, bar) = statistic_permu_with_bar(n_permu, false);

        while (i < n_permu) {
            random_shuffle(row_loc);
            table_do(i, n, row_loc, col_loc, statistic_func, statistic_permu, bar, data);
        }
    }

    bar.done();

    return statistic_permu;
}
