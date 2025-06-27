template <bool progress, typename T>
RObject impl_table_pmt(
    IntegerMatrix data,
    const T& statistic_func,
    const double n_permu)
{
    Stat<progress> statistic_container;

    R_xlen_t r = data.nrow();
    R_xlen_t c = data.ncol();

    std::size_t n = 0;
    for (R_xlen_t k = 0; k < data.size(); k++) {
        n += data[k];
    }

    std::vector<R_xlen_t> row;
    std::vector<R_xlen_t> col;
    row.reserve(n);
    col.reserve(n);
    for (R_xlen_t j = 0; j < c; j++) {
        for (R_xlen_t i = 0; i < r; i++) {
            for (int k = 0; k < data(i, j); k++) {
                row.emplace_back(i);
                col.emplace_back(j);
            }
        }
    }

    auto data_ = [&row, &col, data, n]() mutable {
        for (R_xlen_t k = 0; k < data.size(); k++) {
            data[k] = 0;
        };

        for (std::size_t k = 0; k < n; k++) {
            data(row[k], col[k])++;
        }

        return data;
    };

    auto table_update = [&statistic_container, statistic_closure = statistic_func(data_()), &data_]() {
        return statistic_container << statistic_closure(data_());
    };

    if (std::isnan(n_permu)) {
        statistic_container.init(table_update, 1);
    } else if (n_permu == 0) {
        auto& row_ = n_permutation(row) < n_permutation(col) ? row : col;

        statistic_container.init(table_update, 1, n_permutation(row_));

        while (table_update()) {
            next_permutation(row_);
        }

    } else {
        statistic_container.init(table_update, 1, n_permu);

        do {
            random_shuffle(row);
        } while (table_update());
    }

    return static_cast<RObject>(statistic_container);
}