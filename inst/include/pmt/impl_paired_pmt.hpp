#pragma once

template <bool progress, typename T>
RObject impl_paired_pmt(
    NumericVector x,
    NumericVector y,
    T&& statistic_func,
    const double n_permu)
{
    Stat<progress> statistic_container;

    auto statistic_closure = statistic_func(x, y);
    auto paired_update = [&statistic_container, &statistic_closure, x, y]() {
        return statistic_container << statistic_closure(x, y);
    };

    R_xlen_t n = x.size();

    statistic_container.allocate(1, n_permu != 0 ? n_permu : 1 << n);

#ifdef SETJMP
    SETJMP(statistic_func)
#endif

    paired_update();

    if (!std::isnan(n_permu)) {
        for (R_xlen_t i = 0; i < n; i++) {
            if (x[i] == y[i]) {
                while (--n > i && x[n] == y[n]) { }
                std::swap(x[i], x[n]);
                std::swap(y[i], y[n]);
            }
        }

        statistic_container.switch_ptr();
        if (n_permu == 0) {
            R_xlen_t swapped = 0;
            for (R_xlen_t i = 0; i < n; i = swapped & (1 << i) ? 0 : i + 1) {
                if (i == 0) {
                    paired_update();
                }

                std::swap(x[i], y[i]);
                swapped ^= (1 << i);
            }
        } else {
            do {
                for (R_xlen_t i = 0; i < n; i++) {
                    swap_if(unif_rand() > 0.5, x[i], y[i]);
                }
            } while (paired_update());
        }
    }

    return static_cast<RObject>(statistic_container);
}