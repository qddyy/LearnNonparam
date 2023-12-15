#include "utils.h"

using namespace Rcpp;

inline bool rcbd_update(
    PermuBar& bar,
    const NumericMatrix& data,
    const Function& statistic_func
    )
{
    return bar.update(as<double>(statistic_func(data)));
}

// [[Rcpp::export]]
NumericVector rcbd_pmt(
    NumericMatrix data,
    const Function statistic_func,
    const unsigned n_permu)
{
    unsigned n_col = data.ncol();

    unsigned i = 0;
    if (n_permu == 0) {
        unsigned total = 1;
        for (unsigned j = 0; j < n_col; j++) {
            total *= n_permutation(data.column(j));
        }

        PermuBar bar(total, true);

        while (i < n_col) {
            if (i == 0) {
                rcbd_update(bar, data, statistic_func);
            }

            if (std::next_permutation(data.column(i).begin(), data.column(i).end())) {
                i = 0;
            } else {
                i++;
            }
        }

        return bar.statistic_permu;
    } else {
        PermuBar bar(n_permu, false);

        do {
            for (i = 0; i < n_col; i++) {
                random_shuffle(data.column(i));
            }
        } while (rcbd_update(bar, data, statistic_func));

        return bar.statistic_permu;
    }
}
