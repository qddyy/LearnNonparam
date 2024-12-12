template <bool progress, typename T>
RObject impl_association_pmt(
    NumericVector x,
    NumericVector y,
    const T& statistic_func,
    const double n_permu)
{
    Stat<progress> statistic_container;

    auto statistic_closure = statistic_func(x, y);
    auto association_update = [x, y, &statistic_closure, &statistic_container]() {
        return statistic_container << statistic_closure(x, y);
    };

    statistic_container.init_statistic(association_update);

    if (!std::isnan(n_permu)) {
        if (n_permu == 0) {
            std::sort(x.begin(), x.end());
            std::sort(y.begin(), y.end());

            NumericVector y_ = (n_permutation(x) < n_permutation(y)) ? x : y;

            statistic_container.init_statistic_permu(n_permutation(y_));

            do {
                association_update();
            } while (next_permutation(y_));
        } else {
            statistic_container.init_statistic_permu(n_permu);

            do {
                random_shuffle(y);
            } while (association_update());
        }
    }

    return statistic_container.close();
}