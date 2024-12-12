template <bool progress, typename T>
RObject impl_ksample_pmt(
    const NumericVector data,
    IntegerVector group,
    const T& statistic_func,
    const double n_permu)
{
    Stat<progress> statistic_container;

    auto statistic_closure = statistic_func(data, group);
    auto ksample_update = [data, group, &statistic_closure, &statistic_container]() {
        return statistic_container << statistic_closure(data, group);
    };

    statistic_container.init_statistic(ksample_update);

    if (!std::isnan(n_permu)) {
        if (n_permu == 0) {
            statistic_container.init_statistic_permu(n_permutation(group));

            do {
                ksample_update();
            } while (next_permutation(group));
        } else {
            statistic_container.init_statistic_permu(n_permu);

            do {
                random_shuffle(group);
            } while (ksample_update());
        }
    }

    return statistic_container.close();
}