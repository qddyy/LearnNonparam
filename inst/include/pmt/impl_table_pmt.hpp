template <typename T, typename U>
NumericVector impl_table_pmt(
    IntegerVector row_loc,
    const IntegerVector col_loc,
    const U statistic_func,
    const R_xlen_t n_permu)
{
    T bar;

    R_len_t n = row_loc.size();

    IntegerMatrix data(no_init(row_loc[n - 1] + 1, col_loc[n - 1] + 1));

    auto data_filled = [data, row_loc, col_loc, n]() mutable {
        data.fill(0);
        for (R_len_t i = 0; i < n; i++) {
            data(row_loc[i], col_loc[i])++;
        }
        return data;
    };

    auto statistic_closure = statistic_func(data_filled());
    auto table_update = [&data_filled, &statistic_closure, &bar]() {
        return bar << statistic_closure(data_filled());
    };

    if (n_permu == 0) {
        bar.init(n_permutation(row_loc), table_update);

        do {
            table_update();
        } while (next_permutation(row_loc));
    } else {
        bar.init(n_permu, table_update);

        do {
            random_shuffle(row_loc);
        } while (table_update());
    }

    return bar.close();
}