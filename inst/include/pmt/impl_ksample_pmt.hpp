template <typename T, typename U>
NumericVector impl_ksample_pmt(
    const NumericVector data,
    IntegerVector group,
    const U& statistic_func,
    const R_xlen_t n_permu)
{
    T bar;

    auto statistic_closure = statistic_func(data, group);
    auto ksample_update = [data, group, &statistic_closure, &bar]() {
        return bar << statistic_closure(data, group);
    };

    bar.init_statistic(ksample_update);
    if (n_permu == 0) {
        bar.init_statistic_permu(n_permutation(group));

        do {
            ksample_update();
        } while (next_permutation(group));
    } else {
        bar.init_statistic_permu(n_permu);

        do {
            random_shuffle(group);
        } while (ksample_update());
    }

    return bar.close();
}