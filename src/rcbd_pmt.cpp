#include "progress.h"
#include "utils.h"

template <typename T>
NumericVector rcbd_pmt_impl(
    NumericMatrix data,
    const Function& statistic_func,
    const R_xlen_t n_permu)
{
    auto rcbd_update = [&](T& bar) -> bool {
        return bar.update(as<double>(statistic_func(data)));
    };

    R_len_t i = 0;
    R_len_t n_col = data.ncol();
    if (n_permu == 0) {
        R_xlen_t total = 1;
        for (R_len_t j = 0; j < n_col; j++) {
            total *= n_permutation(data.column(j));
        }

        T bar(total, true);

        while (i < n_col) {
            if (i == 0) {
                rcbd_update(bar);
            }

            if (std::next_permutation(data.column(i).begin(), data.column(i).end())) {
                i = 0;
            } else {
                i++;
            }
        }

        return bar.statistic_permu;
    } else {
        T bar(n_permu, false);

        do {
            for (i = 0; i < n_col; i++) {
                random_shuffle(data.column(i));
            }
        } while (rcbd_update(bar));

        return bar.statistic_permu;
    }
}

// [[Rcpp::export]]
NumericVector rcbd_pmt(
    const NumericMatrix data,
    const Function statistic_func,
    const R_xlen_t n_permu,
    const bool progress)
{
    if (progress) {
        return rcbd_pmt_impl<PermuBarAppear>(data, statistic_func, n_permu);
    } else {
        return rcbd_pmt_impl<PermuBarDisappear>(data, statistic_func, n_permu);
    }
}