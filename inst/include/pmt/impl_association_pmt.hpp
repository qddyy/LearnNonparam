template <typename T, typename U>
NumericVector impl_association_pmt(
    NumericVector x,
    NumericVector y,
    const U& statistic_func,
    const double n_permu)
{
    T bar;

    auto statistic_closure = statistic_func(x, y);
    auto association_update = [x, y, &statistic_closure, &bar]() {
        return bar << statistic_closure(x, y);
    };

    bar.init_statistic(association_update);

    if (!std::isnan(n_permu)) {
        if (n_permu == 0) {
            std::sort(x.begin(), x.end());
            std::sort(y.begin(), y.end());

            NumericVector y_ = (n_permutation(x) < n_permutation(y)) ? x : y;

            bar.init_statistic_permu(n_permutation(y_));

            do {
                association_update();
            } while (next_permutation(y_));
        } else {
            bar.init_statistic_permu(n_permu);

            do {
                random_shuffle(y);
            } while (association_update());
        }
    }

    return bar.close();
}