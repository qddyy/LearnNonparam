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

#ifdef SETJMP
    SETJMP(statistic_func)
#endif
    if (std::isnan(n_permu)) {
        statistic_container.init(paired_update, 1);
    } else {
        R_xlen_t n = x.size();

        for (R_xlen_t i = 0; i < n; i++) {
            if (x[i] == y[i]) {
                while (--n > i && x[n] == y[n]) { }
                std::swap(x[i], x[n]);
                std::swap(y[i], y[n]);
            }
        }

        if (n_permu == 0) {
            statistic_container.init(paired_update, 1, 1 << n);

            R_xlen_t swapped = 0;
            for (R_xlen_t i = 0; i < n; i = swapped & (1 << i) ? 0 : i + 1) {
                if (i == 0) {
                    paired_update();
                }

                std::swap(x[i], y[i]);
                swapped ^= (1 << i);
            }
        } else {
            statistic_container.init(paired_update, 1, n_permu);

            do {
                for (R_xlen_t i = 0; i < n; i++) {
                    swap_if(unif_rand() > 0.5, x[i], y[i]);
                }
            } while (paired_update());
        }
    }

    return static_cast<RObject>(statistic_container);
}