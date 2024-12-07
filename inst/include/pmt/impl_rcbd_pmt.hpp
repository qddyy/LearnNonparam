template <typename T, typename U>
NumericVector impl_rcbd_pmt(
    NumericMatrix data,
    const U& statistic_func,
    const double n_permu)
{
    T bar;

    auto statistic_closure = statistic_func(data);
    auto rcbd_update = [data, &statistic_closure, &bar]() {
        return bar << statistic_closure(data);
    };

    bar.init_statistic(rcbd_update);

    if (!std::isnan(n_permu)) {
        R_len_t i;
        R_len_t b = data.ncol();

        if (n_permu == 0) {
            double total = 1;
            for (i = 0; i < b; i++) {
                std::sort(data.column(i).begin(), data.column(i).end());
                total *= n_permutation(data.column(i));
            }

            bar.init_statistic_permu(total);

            i = 0;
            while (i < b) {
                if (i == 0) {
                    rcbd_update();
                }

                i = next_permutation(data.column(i)) ? 0 : i + 1;
            }
        } else {
            bar.init_statistic_permu(n_permu);

            do {
                for (i = 0; i < b; i++) {
                    random_shuffle(data.column(i));
                }
            } while (rcbd_update());
        }
    }

    return bar.close();
}