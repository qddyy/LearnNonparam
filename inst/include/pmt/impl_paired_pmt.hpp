template <bool progress, typename T>
RObject impl_paired_pmt(
    NumericVector x,
    NumericVector y,
    const T& statistic_func,
    const double n_permu)
{
    Stat<progress> statistic_container;

    auto paired_update = [&statistic_container, statistic_closure = statistic_func(x, y), x, y]() {
        return statistic_container << statistic_closure(x, y);
    };

    if (std::isnan(n_permu)) {
        statistic_container.init(paired_update, 1);
    } else {
        R_xlen_t n = x.size();

        R_xlen_t i;

        for (i = 0; i < n; i++) {
            if (x[i] == y[i]) {
                while (--n > i && x[n] == y[n]) { }
                std::swap(x[i], x[n]);
                std::swap(y[i], y[n]);
            }
        }

        if (n_permu == 0) {
            statistic_container.init(paired_update, 1, 1 << n);

            R_xlen_t swapped = 0;

            i = 0;
            while (i < n) {
                if (i == 0) {
                    paired_update();
                }

                std::swap(x[i], y[i]);
                swapped ^= (1 << i);
                if (swapped & (1 << i)) {
                    i = 0;
                    continue;
                }

                i++;
            }
        } else {
            statistic_container.init(paired_update, 1, n_permu);

            do {
                for (i = 0; i < n; i++) {
                    if (rand_int(2) == 1) {
                        std::swap(x[i], y[i]);
                    }
                }
            } while (paired_update());
        }
    }

    return static_cast<RObject>(statistic_container);
}