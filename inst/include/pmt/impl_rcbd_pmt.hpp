template <bool progress, typename T>
RObject impl_rcbd_pmt(
    NumericMatrix data,
    const T& statistic_func,
    const double n_permu)
{
    Stat<progress> statistic_container;

    auto statistic_closure = statistic_func(data);
    auto rcbd_update = [data, &statistic_closure, &statistic_container]() {
        return statistic_container << statistic_closure(data);
    };

    statistic_container.init_statistic(rcbd_update);

    if (!std::isnan(n_permu)) {
        R_xlen_t k = data.nrow();

        auto begin = data.begin();
        auto end = data.end() - k;

        decltype(end) it;
        if (n_permu == 0) {
            double total = 1.0;
            for (it = begin; it != end; it += k) {
                std::sort(it, it + k);
                total *= n_permutation(it, it + k);
            }

            statistic_container.init_statistic_permu(total);

            it = begin;
            while (it != end) {
                if (it == begin) {
                    rcbd_update();
                }

                it = next_permutation(it, it + k) ? begin : it + k;
            }
        } else {
            statistic_container.init_statistic_permu(n_permu);

            do {
                for (it = begin; it != end; it += k) {
                    random_shuffle(it, it + k);
                }
            } while (rcbd_update());
        }
    }

    return statistic_container.close();
}