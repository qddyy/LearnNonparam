template <typename T, typename U>
NumericVector impl_rcbd_pmt(
    NumericMatrix data,
    const U statistic_func,
    const R_xlen_t n_permu)
{
    T bar;

    auto statistic_closure = statistic_func(data);
    auto rcbd_update = [data, &statistic_closure, &bar]() {
        return bar << statistic_closure(data);
    };

    R_len_t i = 0;
    R_len_t n_col = data.ncol();
    if (n_permu == 0) {
        R_xlen_t total = 1;
        for (R_len_t j = 0; j < n_col; j++) {
            std::sort(data.column(j).begin(), data.column(j).end());
            total *= n_permutation(data.column(j));
        }

        bar.init(total, rcbd_update);

        while (i < n_col) {
            if (i == 0) {
                rcbd_update();
            }

            if (next_permutation(data.column(i))) {
                i = 0;
            } else {
                i++;
            }
        }
    } else {
        bar.init(n_permu, rcbd_update);

        do {
            for (i = 0; i < n_col; i++) {
                random_shuffle(data.column(i));
            }
        } while (rcbd_update());
    }

    return bar.close();
}