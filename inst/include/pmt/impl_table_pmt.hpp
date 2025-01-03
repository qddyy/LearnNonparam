template <bool progress, typename T>
RObject impl_table_pmt(
    IntegerVector row,
    IntegerVector col,
    const T& statistic_func,
    const double n_permu)
{
    Stat<progress> statistic_container;

    auto data = [_data = IntegerMatrix(no_init(*(row.end() - 1) + 1, *(col.end() - 1) + 1)), row, col, n = row.size()]() mutable {
        _data.fill(0);
        for (R_xlen_t i = 0; i < n; i++) {
            _data(row[i], col[i])++;
        }
        return _data;
    };

    auto table_update = [&statistic_container, statistic_closure = statistic_func(data()), &data]() {
        return statistic_container << statistic_closure(data());
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