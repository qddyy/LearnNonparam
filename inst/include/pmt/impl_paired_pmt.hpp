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

    statistic_container.init_statistic(paired_update);

    if (!std::isnan(n_permu)) {
        R_xlen_t n = x.size();

        R_xlen_t i;
        if (n_permu == 0) {
            double total = 1.0;
            for (i = 0; i < n; i++) {
                if (x[i] != y[i]) {
                    total *= 2;
                    if (x[i] > y[i]) {
                        std::swap(x[i], y[i]);
                    }
                }
            }

            statistic_container.init_statistic_permu(total);

            i = 0;
            while (i < n) {
                if (i == 0) {
                    paired_update();
                }

                if (x[i] != y[i]) {
                    std::swap(x[i], y[i]);
                    if (x[i] > y[i]) {
                        i = 0;
                        continue;
                    }
                }

                i++;
            }
        } else {
            statistic_container.init_statistic_permu(n_permu);

            do {
                for (i = 0; i < n; i++) {
                    if (x[i] != y[i] && rand_int(2) == 1) {
                        std::swap(x[i], y[i]);
                    }
                }
            } while (paired_update());
        }
    }

    return statistic_container.close();
}