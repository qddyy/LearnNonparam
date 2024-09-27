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
        R_len_t i = 0;
        R_len_t n_block = data.ncol();

        if (n_permu == 0) {
            double total = 1;
            for (R_len_t j = 0; j < n_block; j++) {
                std::sort(data.column(j).begin(), data.column(j).end());
                total *= n_permutation(data.column(j));
            }

            bar.init_statistic_permu(total);

            while (i < n_block) {
                if (i == 0) {
                    rcbd_update();
                }

                if (next_permutation(data.column(i))) {
                    i = 0;
                } else {
                    i++;
                }
            }
        } else {
            bar.init_statistic_permu(n_permu);

            do {
                for (i = 0; i < n_block; i++) {
                    random_shuffle(data.column(i));
                }
            } while (rcbd_update());
        }
    }

    return bar.close();
}