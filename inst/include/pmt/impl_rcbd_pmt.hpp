#pragma once

template <bool progress, typename T>
RObject impl_rcbd_pmt(
    NumericMatrix data,
    T&& statistic_func,
    const double n_permu)
{
    Stat<progress> statistic_container;

    auto statistic_closure = statistic_func(data);
    auto rcbd_update = [&statistic_container, &statistic_closure, data]() {
        return statistic_container << statistic_closure(data);
    };

    R_xlen_t k = data.nrow();

    statistic_container.allocate(
        1, n_permu != 0 ? n_permu : [data, k]() mutable {
            double A = 1.0;
            for (auto it = data.begin(); it != data.end(); it += k) {
                std::sort(it, it + k);
                A *= n_permutation(it, it + k);
            }
            return A;
        }());

#ifdef SETJMP
    SETJMP(statistic_func)
#endif

    rcbd_update();

    if (!std::isnan(n_permu)) {
        statistic_container.switch_ptr();
        if (n_permu == 0) {
            for (auto it = data.begin(); it != data.end(); it = next_permutation(it, it + k) ? data.begin() : it + k) {
                if (it == data.begin()) {
                    rcbd_update();
                }
            }
        } else {
            do {
                for (auto it = data.begin(); it != data.end(); it += k) {
                    random_shuffle(it, it + k);
                }
            } while (rcbd_update());
        }
    }

    return static_cast<RObject>(statistic_container);
}