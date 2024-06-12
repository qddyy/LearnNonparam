template <typename T, typename U>
NumericVector impl_twosample_pmt(
    NumericVector x,
    NumericVector y,
    const U statistic_func,
    const R_xlen_t n_permu)
{
    T bar;

    R_len_t m = x.size();
    R_len_t n = m + y.size();

    auto statistic_closure = statistic_func(x, y);
    auto twosample_update = [x, y, &statistic_closure, &bar]() {
        return bar << statistic_closure(x, y);
    };

    bar.init_statistic(twosample_update);
    if (n_permu == 0) {
        NumericVector data(no_init(n));
        std::copy(x.begin(), x.end(), data.begin());
        std::copy(y.begin(), y.end(), data.begin() + m);

        LogicalVector where_y(no_init(n));
        std::fill(where_y.begin(), where_y.begin() + m, false);
        std::fill(where_y.begin() + m, where_y.end(), true);

        bar.init_statistic_permu(n_permutation(where_y));

        R_len_t i, j, k;
        do {
            i = j = k = 0;
            do {
                if (where_y[k]) {
                    y[i++] = data[k++];
                } else {
                    x[j++] = data[k++];
                }
            } while (k < n);
            twosample_update();
        } while (next_permutation(where_y));
    } else {
        bar.init_statistic_permu(n_permu);

        R_len_t i, j;
        do {
            for (i = 0; i < m; i++) {
                j = i + rand_int(n - i);
                if (j >= m) {
                    std::swap(x[i], y[j - m]);
                }
            }
        } while (twosample_update());
    }

    return bar.close();
}