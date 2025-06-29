template <bool progress, typename T>
RObject impl_multcomp_pmt(
    const NumericVector data,
    IntegerVector group,
    const T& statistic_func,
    const double n_permu)
{
    Stat<progress> statistic_container;

    int k = *(group.end() - 1);

    auto statistic_closure = statistic_func(data, group);
    auto multcomp_update = [&statistic_container, &statistic_closure, data, group, k]() {
        auto pairwise_closure = statistic_closure(data, group);

        for (int i = 1; i < k - 1; i++) {
            for (int j = i + 1; j <= k; j++) {
                statistic_container << pairwise_closure(i, j);
            }
        }

        return statistic_container << pairwise_closure(k - 1, k);
    };

    if (std::isnan(n_permu)) {
        statistic_container.init(multcomp_update, C(k, 2));
    } else if (n_permu == 0) {
        statistic_container.init(multcomp_update, C(k, 2), n_permutation(group));

        do {
            multcomp_update();
        } while (next_permutation(group));
    } else {
        statistic_container.init(multcomp_update, C(k, 2), n_permu);

        do {
            random_shuffle(group);
        } while (multcomp_update());
    }

    return static_cast<RObject>(statistic_container);
}