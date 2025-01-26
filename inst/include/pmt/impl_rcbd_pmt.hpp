template <bool progress, typename T>
RObject impl_rcbd_pmt(
    NumericMatrix data,
    const T& statistic_func,
    const double n_permu)
{
    Stat<progress> statistic_container;

    auto rcbd_update = [&statistic_container, statistic_closure = statistic_func(data), data]() {
        return statistic_container << statistic_closure(data);
    };

    if (std::isnan(n_permu)) {
        statistic_container.init(rcbd_update, 1);
    } else {
        R_xlen_t k = data.nrow();

        if (n_permu == 0) {
            double n_permu_ = 1.0;
            for (auto it = data.begin(); it != data.end(); it += k) {
                std::sort(it, it + k);
                n_permu_ *= n_permutation(it, it + k);
            }

            statistic_container.init(rcbd_update, 1, n_permu_);

            for (auto it = data.begin(); it != data.end(); it = next_permutation(it, it + k) ? data.begin() : it + k) {
                if (it == data.begin()) {
                    rcbd_update();
                }
            }
        } else {
            statistic_container.init(rcbd_update, 1, n_permu);

            do {
                for (auto it = data.begin(); it != data.end(); it += k) {
                    random_shuffle(it, it + k);
                }
            } while (rcbd_update());
        }
    }

    return static_cast<RObject>(statistic_container);
}