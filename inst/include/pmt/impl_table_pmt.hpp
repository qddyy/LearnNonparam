template <bool progress, typename T>
RObject impl_table_pmt(
    IntegerVector row,
    IntegerVector col,
    const T& statistic_func,
    const double n_permu)
{
    Stat<progress> statistic_container;

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
    auto table_update = [&data_filled, &statistic_closure, &statistic_container]() {
        return statistic_container << statistic_closure(data_filled());
    };

    statistic_container.init_statistic(table_update);

    if (!std::isnan(n_permu)) {
        if (n_permu == 0) {
            std::sort(row.begin(), row.end());

            IntegerVector col_ = (n_permutation(row) < n_permutation(col)) ? row : col;

            statistic_container.init_statistic_permu(n_permutation(col_));

            do {
                table_update();
            } while (next_permutation(col_));
        } else {
            statistic_container.init_statistic_permu(n_permu);

            do {
                random_shuffle(col);
            } while (table_update());
        }
    }

    return statistic_container.close();
}