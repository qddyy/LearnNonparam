#pragma once

template <bool progress, typename T>
RObject impl_multcomp_pmt(
    const NumericVector data,
    IntegerVector group,
    T&& statistic_func,
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

    statistic_container.allocate(C(k, 2), n_permu != 0 ? n_permu : n_permutation(group));

#ifdef SETJMP
    SETJMP(statistic_func)
#endif

    multcomp_update();

    if (!std::isnan(n_permu)) {
        statistic_container.switch_ptr();
        if (n_permu == 0) {
            do {
                multcomp_update();
            } while (next_permutation(group));
        } else {
            do {
                random_shuffle(group);
            } while (multcomp_update());
        }
    }

    return static_cast<RObject>(statistic_container);
}