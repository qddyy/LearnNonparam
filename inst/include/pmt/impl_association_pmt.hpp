#pragma once

template <bool progress, typename T>
RObject impl_association_pmt(
    NumericVector x,
    NumericVector y,
    T&& statistic_func,
    const double n_permu)
{
    Stat<progress> statistic_container;

    if (n_permu == 0 && n_permutation(x) < n_permutation(y)) {
        std::swap(x, y);
    }

    auto statistic_closure = statistic_func(x, y);
    auto association_update = [&statistic_container, &statistic_closure, x, y]() {
        return statistic_container << statistic_closure(x, y);
    };

    statistic_container.allocate(1, n_permu != 0 ? n_permu : n_permutation(y));

#ifdef SETJMP
    SETJMP(statistic_func)
#endif

    association_update();

    if (!std::isnan(n_permu)) {
        statistic_container.switch_ptr();
        if (n_permu == 0) {
            while (association_update()) {
                next_permutation(y);
            }
        } else {
            do {
                random_shuffle(y);
            } while (association_update());
        }
    }

    return static_cast<RObject>(statistic_container);
}