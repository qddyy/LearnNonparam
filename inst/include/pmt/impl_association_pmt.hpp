template <bool progress, typename T>
RObject impl_association_pmt(
    NumericVector x,
    NumericVector y,
    const T& statistic_func,
    const double n_permu)
{
    Stat<progress> statistic_container;

    auto association_update = [&statistic_container, statistic_closure = statistic_func(x, y), x, y]() {
        return statistic_container << statistic_closure(x, y);
    };

    if (std::isnan(n_permu)) {
        statistic_container.init(association_update, 1);
    } else if (n_permu == 0) {
        std::sort(x.begin(), x.end());
        std::sort(y.begin(), y.end());

        NumericVector y_ = n_permutation(x) < n_permutation(y) ? x : y;

        statistic_container.init(association_update, 1, n_permutation(y_));

        do {
            association_update();
        } while (next_permutation(y_));
    } else {
        statistic_container.init(association_update, 1, n_permu);

        do {
            random_shuffle(y);
        } while (association_update());
    }

    return static_cast<RObject>(statistic_container);
}