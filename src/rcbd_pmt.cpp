#include "utils.h"

using namespace Rcpp;

inline void rcbd_do(
    unsigned& i,
    const NumericMatrix& data,
    const Function& statistic_func,
    NumericVector& statistic_permu,
    ProgressBar& bar)
{
    statistic_permu[i] = as<double>(statistic_func(data));

    bar.update(i);
    i++;
}

// [[Rcpp::export]]
NumericVector rcbd_pmt(
    NumericMatrix data,
    const Function statistic_func,
    const unsigned n_permu)
{
    ProgressBar bar;
    NumericVector statistic_permu;

    unsigned n_col = data.ncol();

    unsigned i = 0;
    unsigned j = 0;
    if (n_permu == 0) {
        unsigned total = 1;
        for (unsigned k = 0; k < n_col; k++) {
            total *= n_permutation(data.column(k));
        }
        std::tie(statistic_permu, bar) = statistic_permu_with_bar(total, true);

        while (j < n_col) {
            if (j == 0) {
                rcbd_do(i, data, statistic_func, statistic_permu, bar);
            }

            if (std::next_permutation(data.column(j).begin(), data.column(j).end())) {
                j = 0;
            } else {
                j++;
            }
        }
    } else {
        std::tie(statistic_permu, bar) = statistic_permu_with_bar(n_permu, false);

        while (i < n_permu) {
            for (j = 0; j < n_col; j++) {
                random_shuffle(data.column(j));
            }
            rcbd_do(i, data, statistic_func, statistic_permu, bar);
        }
    }

    bar.done();

    return statistic_permu;
}
