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
        IntegerVector x_order = seq_along(x) - 1;
        sort(x_order, [x](int i, int j) { return x[i] < x[j]; });

        x = x[x_order];
        y = y[x_order];

        bar.init(n_permutation(x), association_update);

        do {
            association_update();
        } while (next_permutation(x));
    } else {
        bar.init(n_permu, association_update);

        do {
            random_shuffle(x);
        } while (association_update());
    }

    return bar.close();
}