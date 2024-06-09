template <typename T, typename U>
NumericVector impl_association_pmt(
    NumericVector x,
    NumericVector y,
    const U statistic_func,
    const R_xlen_t n_permu)
{
    T bar;

    auto statistic_closure = statistic_func(x, y);
    auto association_update = [x, y, &statistic_closure, &bar]() {
        return bar << statistic_closure(x, y);
    };

    if (n_permu == 0) {
        NumericVector y_sorted = clone(y);
        std::sort(y_sorted.begin(), y_sorted.end());

        bar.init(n_permutation(y_sorted), association_update);

        std::copy(y_sorted.begin(), y_sorted.end(), y.begin());

        do {
            association_update();
        } while (next_permutation(y));
    } else {
        bar.init(n_permu, association_update);

        do {
            random_shuffle(y);
        } while (association_update());
    }

    return bar.close();
}