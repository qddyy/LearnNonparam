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

    statistic_container.allocate(1, n_permu != 0 ? n_permu : n_permutation(group));

#ifdef SETJMP
    SETJMP(statistic_func)
#endif

    ksample_update();

    if (!std::isnan(n_permu)) {
        statistic_container.switch_ptr();
        if (n_permu == 0) {
            do {
                ksample_update();
            } while (next_permutation(group));
        } else {
            do {
                random_shuffle(group);
            } while (ksample_update());
        }
    }

    return static_cast<RObject>(statistic_container);
}

template <bool progress, typename T>
RObject impl_multcomp_pmt(
    const NumericVector data,
    IntegerVector group,
    T&& statistic_func,
    const double n_permu)
{
    Stat<progress> statistic_container;

    int c = C(*std::prev(group.end()), 2);

    auto statistic_closure = statistic_func(data, group);
    auto multcomp_update = [&statistic_container, &statistic_closure, data, group, c]() {
        const double* statistic_ptr = statistic_closure(data, group);

        for (int i = 1; i < c; i++) {
            statistic_container << *statistic_ptr++;
        }

        return statistic_container << *statistic_ptr;
    };

    statistic_container.allocate(c, n_permu != 0 ? n_permu : n_permutation(group));

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