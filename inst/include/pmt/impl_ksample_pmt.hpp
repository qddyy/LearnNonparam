#pragma once

template <bool progress, typename T>
RObject impl_ksample_pmt(
    const NumericVector data,
    IntegerVector group,
    T&& statistic_func,
    const double n_permu)
{
    Stat<progress> statistic_container;

    auto statistic_closure = statistic_func(data, group);
    auto ksample_update = [&statistic_container, &statistic_closure, data, group]() {
        return statistic_container << statistic_closure(data, group);
    };

#ifdef SETJMP
    SETJMP(statistic_func)
#endif
    if (std::isnan(n_permu)) {
        statistic_container.init(ksample_update, 1);
    } else if (n_permu == 0) {
        statistic_container.init(ksample_update, 1, n_permutation(group));

        do {
            ksample_update();
        } while (next_permutation(group));
    } else {
        statistic_container.init(ksample_update, 1, n_permu);

        do {
            random_shuffle(group);
        } while (ksample_update());
    }

    return static_cast<RObject>(statistic_container);
}