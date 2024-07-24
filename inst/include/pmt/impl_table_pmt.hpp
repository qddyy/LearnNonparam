template <typename T, typename U>
NumericVector impl_table_pmt(
    const IntegerVector row,
    IntegerVector col,
    const U& statistic_func,
    const std::string type,
    const R_xlen_t n_permu)
{
    T bar;

    R_len_t n = row.size();

    IntegerMatrix data(no_init(row[n - 1] + 1, col[n - 1] + 1));

    auto data_filled = [data, row, col, n]() mutable {
        data.fill(0);
        for (R_len_t i = 0; i < n; i++) {
            data(row[i], col[i])++;
        }
        return data;
    };

    auto statistic_closure = statistic_func(data_filled());
    auto table_update = [&data_filled, &statistic_closure, &bar]() {
        return bar << statistic_closure(data_filled());
    };

    bar.init_statistic(table_update);

    if (type != "permu") {
        return bar.close();
    }

    if (n_permu == 0) {
        bar.init_statistic_permu(n_permutation(col));

        do {
            table_update();
        } while (next_permutation(col));
    } else {
        bar.init_statistic_permu(n_permu);

        do {
            random_shuffle(col);
        } while (table_update());
    }

    return bar.close();
}