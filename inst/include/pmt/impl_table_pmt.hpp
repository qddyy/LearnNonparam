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

    if (std::isnan(n_permu)) {
        statistic_container.init(table_update, 1);
    } else if (n_permu == 0) {
        IntegerVector col_ = n_permutation(row) < n_permutation(col) ? row : col;

        statistic_container.init(table_update, 1, n_permutation(col_));

        while (table_update()) {
            next_permutation(col_);
        }

    } else {
        statistic_container.init(table_update, 1, n_permu);

        do {
            random_shuffle(col);
        } while (table_update());
    }

    return static_cast<RObject>(statistic_container);
}