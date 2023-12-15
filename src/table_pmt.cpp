#include "utils.h"

using namespace Rcpp;

inline bool table_update(
    PermuBar& bar,
    const unsigned& n,
    const IntegerVector& row_loc,
    const IntegerVector& col_loc,
    const Function& statistic_func,
    IntegerMatrix& data)
{
    data.fill(0);

    for (unsigned i = 0; i < n; i++) {
        data(row_loc[i], col_loc[i])++;
    }

    return bar.update(as<double>(statistic_func(data)));
}

// [[Rcpp::export]]
NumericVector table_pmt(
    IntegerVector row_loc,
    const IntegerVector col_loc,
    const Function statistic_func,
    const unsigned n_permu)
{
    unsigned n = row_loc.size();

    IntegerMatrix data(no_init(row_loc[n - 1] + 1, col_loc[n - 1] + 1));

    if (n_permu == 0) {
        PermuBar bar(n_permutation(row_loc), true);

        do {
            table_update(bar, n, row_loc, col_loc, statistic_func, data);
        } while (std::next_permutation(row_loc.begin(), row_loc.end()));

        return bar.statistic_permu;
    } else {
        PermuBar bar(n_permu, false);

        do {
            random_shuffle(row_loc);
        } while (table_update(bar, n, row_loc, col_loc, statistic_func, data));

        return bar.statistic_permu;
    }
}
