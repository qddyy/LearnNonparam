template <typename T, typename U>
NumericVector impl_paired_pmt(
    NumericVector x,
    NumericVector y,
    const U statistic_func,
    const R_xlen_t n_permu)
{
    T bar;

    auto statistic_closure = statistic_func(x, y);
    auto paired_update = [x, y, &statistic_closure, &bar]() {
        return bar << statistic_closure(x, y);
    };

    R_len_t i = 0;
    R_len_t n = x.size();
    bar.init_statistic(paired_update);
    if (n_permu == 0) {
        bar.init_statistic_permu(1 << n);

        IntegerVector swapped(n, 0);
        while (i < n) {
            if (i == 0) {
                paired_update();
            }

            std::swap(x[i], y[i]);
            swapped[i]++;

            if (swapped[i] < 2) {
                i = 0;
            } else {
                swapped[i++] = 0;
            }
        }
    } else {
        bar.init_statistic_permu(n_permu);

        do {
            for (i = 0; i < n; i++) {
                if (rand_int(2) == 1) {
                    std::swap(x[i], y[i]);
                }
            }
        } while (paired_update());
    }

    return bar.close();
}