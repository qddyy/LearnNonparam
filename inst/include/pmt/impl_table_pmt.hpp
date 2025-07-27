#pragma once

template <bool progress, typename T>
RObject impl_table_pmt(
    IntegerMatrix data,
    const T& statistic_func,
    const double n_permu)
{
    Stat<progress> statistic_container;

    std::size_t n = 0;
    for (R_xlen_t k = 0; k < data.size(); k++) {
        n += data[k];
    }

    std::vector<R_xlen_t> row;
    std::vector<R_xlen_t> col;
    row.reserve(n);
    col.reserve(n);
    for (R_xlen_t j = 0; j < data.ncol(); j++) {
        for (R_xlen_t i = 0; i < data.nrow(); i++) {
            for (int k = 0; k < data(i, j); k++) {
                row.emplace_back(i);
                col.emplace_back(j);
            }
        }
    }

    auto statistic_closure_ = [statistic_closure = statistic_func(data), &row, &col, data, n](auto&&... args) mutable {
        for (R_xlen_t k = 0; k < data.size(); k++) {
            data[k] = 0;
        };

        for (std::size_t k = 0; k < n; k++) {
            data(row[k], col[k])++;
        }

        return statistic_closure(std::forward<decltype(args)>(args)...);
    };
    auto table_update = [&statistic_container, &statistic_closure_, data]() {
        return statistic_container << statistic_closure_(data);
    };

    if (std::isnan(n_permu)) {
        statistic_container.init(table_update, 1);
    } else if (n_permu == 0) {
        std::vector<R_xlen_t>& row_ = n_permutation(row) < n_permutation(col) ? row : col;

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

template <bool progress, typename T>
RObject impl_distribution_pmt(
    const NumericVector x,
    const NumericVector y,
    const T& statistic_func,
    const double n_permu)
{
    R_xlen_t m = x.size();
    R_xlen_t n = y.size();

    std::map<double, std::pair<R_xlen_t, R_xlen_t>> freq;
    for (R_xlen_t i = 0; i < m; i++) {
        freq[x[i]].first++;
    }
    for (R_xlen_t i = 0; i < n; i++) {
        freq[y[i]].second++;
    }

    IntegerMatrix data(no_init(freq.size(), 2));
    std::transform(freq.begin(), freq.end(), data.column(0).begin(), [](const auto& x) { return x.second.first; });
    std::transform(freq.begin(), freq.end(), data.column(1).begin(), [](const auto& x) { return x.second.second; });

    NumericVector F(no_init(data.nrow() + 1));
    NumericVector G(no_init(data.nrow() + 1));
    *F.begin() = *G.begin() = 0.0;

    auto build_ecdf = [m, n, F, G, data_F = data.column(0), data_G = data.column(1)]() mutable {
        std::partial_sum(data_F.begin(), data_F.end(), F.begin() + 1);
        std::partial_sum(data_G.begin(), data_G.end(), G.begin() + 1);
        std::transform(F.begin() + 1, F.end(), F.begin() + 1, [m](double x) { return x / m; });
        std::transform(G.begin() + 1, G.end(), G.begin() + 1, [n](double x) { return x / n; });
    };

    auto distribution_decorator = [&build_ecdf, F, G](auto&& func) {
        return [func = std::forward<decltype(func)>(func), &build_ecdf, F, G](auto&&...) {
            build_ecdf();
            return func(F, G);
        };
    };

    auto statistic_func_ = [statistic_func = distribution_decorator(statistic_func), &distribution_decorator](auto&&...) {
        return distribution_decorator(statistic_func());
    };

    return impl_table_pmt<progress>(data, statistic_func_, n_permu);
}