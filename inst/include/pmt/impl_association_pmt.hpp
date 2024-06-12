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

    bar.init_statistic(association_update);
    if (n_permu == 0) {
        std::sort(y.begin(), y.end());

        bar.init_statistic_permu(n_permutation(y));

        do {
            association_update();
        } while (next_permutation(y));
    } else {
        bar.init_statistic_permu(n_permu);

        do {
            random_shuffle(y);
        } while (association_update());
    }

    return bar.close();
}