#pragma once

template <typename T>
struct table_traits;

template <bool progress, typename T, typename U>
RObject __impl_table_pmt(
    T& data,
    U&& statistic_func,
    const double n_permu)
{
    Stat<progress> statistic_container;

    using size_t = typename table_traits<T>::size_type;
    using value_t = typename table_traits<T>::value_type;

    std::size_t n = 0;
    for (size_t k = 0; k < data.size(); k++) {
        n += static_cast<std::size_t>(data[k]);
    }

    std::vector<size_t> row;
    std::vector<size_t> col;
    row.reserve(n);
    col.reserve(n);
    for (size_t j = 0; j < data.ncol(); j++) {
        for (size_t i = 0; i < data.nrow(); i++) {
            for (value_t k = 0; k < data(i, j); k++) {
                row.emplace_back(i);
                col.emplace_back(j);
            }
        }
    }

    auto statistic_closure_ = [statistic_closure = statistic_func(data), &data, &row, &col, n](auto&&... args) mutable {
        for (size_t k = 0; k < data.size(); k++) {
            data[k] = 0;
        }

        for (std::size_t k = 0; k < n; k++) {
            data(row[k], col[k])++;
        }

        return statistic_closure(std::forward<decltype(args)>(args)...);
    };
    auto table_update = [&statistic_container, &statistic_closure_, data]() {
        return statistic_container << statistic_closure_(data);
    };

#ifdef SETJMP
    SETJMP(statistic_func)
#endif
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

template <>
struct table_traits<IntegerMatrix> {
    using size_type = R_xlen_t;
    using value_type = int;
};

template <bool progress, typename T>
RObject impl_table_pmt(
    IntegerMatrix data,
    T&& statistic_func,
    const double n_permu)
{
    return __impl_table_pmt<progress>(data, statistic_func, n_permu);
}

class DistributionTable : public std::vector<double> {
public:
    DistributionTable(const NumericVector x, const NumericVector y)
    {
        std::map<double, std::pair<double, double>> freq;
        for (R_xlen_t i = 0; i < x.size(); i++) {
            freq[x[i]].first++;
        }
        for (R_xlen_t i = 0; i < y.size(); i++) {
            freq[y[i]].second++;
        }

        _ncol = freq.size();
        if (_ncol >= R_XLEN_T_MAX) {
            stop("ECDF would be too long a vector");
        }

        std::vector<double>::reserve(_ncol * 2);
        for (auto it = freq.begin(); it != freq.end(); it++) {
            std::vector<double>::push_back(it->second.first);
            std::vector<double>::push_back(it->second.second);
        }
    }

    std::size_t nrow() const { return 2; }

    std::size_t ncol() const { return _ncol; }

    double& operator()(std::size_t i, std::size_t j)
    {
        return std::vector<double>::operator[](i + j * 2);
    }

private:
    std::size_t _ncol;
};

template <typename T, typename U>
class DistributionFunc : public T {
public:
    template <typename V, typename = std::enable_if_t<std::is_same<std::remove_cv_t<std::remove_reference_t<V>>, T>::value>>
    DistributionFunc(V&& func, const U& data, R_xlen_t n_x, R_xlen_t n_y) :
        T(std::forward<V>(func)),
        _F(no_init(data.ncol() + 1)),
        _G(no_init(data.ncol() + 1)),
        _inv_n_x(1.0 / static_cast<double>(n_x)),
        _inv_n_y(1.0 / static_cast<double>(n_y)),
        _data(data)
    {
        *_F.begin() = *_G.begin() = 0.0;
    }

    template <typename... Args>
    auto operator()(Args&&...)
    {
        _build_ecdf();
        return [closure = T::operator()(_F, _G), this](auto&&...) {
            this->_build_ecdf();
            return closure(this->_F, this->_G);
        };
    }

private:
    void _build_ecdf()
    {
        double prob_F = 0.0;
        double prob_G = 0.0;

        std::size_t k = 0;
        for (R_xlen_t i = 1; i < _G.size(); i++) {
            prob_F += _data[k++] * _inv_n_x;
            prob_G += _data[k++] * _inv_n_y;
            _F[i] = prob_F;
            _G[i] = prob_G;
        }
    }

    NumericVector _F;
    NumericVector _G;

    double _inv_n_x;
    double _inv_n_y;

    const U& _data;
};

template <>
struct table_traits<DistributionTable> {
    using size_type = std::size_t;
    using value_type = double;
};

template <bool progress, typename T>
RObject impl_distribution_pmt(
    const NumericVector x,
    const NumericVector y,
    T&& statistic_func,
    const double n_permu)
{
    DistributionTable data(x, y);

    DistributionFunc<std::remove_cv_t<std::remove_reference_t<T>>, DistributionTable> statistic_func_(std::forward<T>(statistic_func), data, x.size(), y.size());

    return __impl_table_pmt<progress>(data, statistic_func_, n_permu);
}