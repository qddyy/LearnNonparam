template <bool progress, typename T>
RObject impl_paired_pmt(
    NumericVector x,
    NumericVector y,
    const T& statistic_func,
    const double n_permu)
{
    Stat<progress> statistic_container;

    auto statistic_closure = statistic_func(x, y);
    auto paired_update = [x, y, &statistic_closure, &statistic_container]() {
        return statistic_container << statistic_closure(x, y);
    };

    statistic_container.init_statistic(paired_update);

    if (!std::isnan(n_permu)) {
        R_xlen_t n = x.size();

        R_xlen_t i;
        if (n_permu == 0) {
            statistic_container.init_statistic_permu(1 << n);

            IntegerVector swapped(n, 0);

            i = 0;
            while (i < n) {
                if (i == 0) {
                    paired_update();
                }

                std::swap(x[i], y[i]);
                swapped[i]++;

                if (swapped[i] < 2) {
                    i = 0;
                } else {
                    swapped[i++] = 0;
                }
            }
        } else {
            statistic_container.init_statistic_permu(n_permu);

            do {
                for (i = 0; i < n; i++) {
                    if (rand_int(2) == 1) {
                        std::swap(x[i], y[i]);
                    }
                }
            } while (paired_update());
        }
    }

    return statistic_container.close();
}