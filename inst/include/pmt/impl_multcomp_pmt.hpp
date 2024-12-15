template <bool progress, typename T>
RObject impl_multcomp_pmt(
    const IntegerVector group_i,
    const IntegerVector group_j,
    const NumericVector data,
    IntegerVector group,
    const T& statistic_func,
    const double n_permu)
{
    R_xlen_t n_group = group[group.size() - 1];
    R_xlen_t n_pair = n_group * (n_group - 1) / 2;

    Stat<progress> statistic_container(n_pair);

    auto multcomp_update = [group_i, group_j, data, group, n_pair, &statistic_func, &statistic_container]() {
        auto statistic_closure = statistic_func(data, group);

        bool flag = false;
        for (R_xlen_t k = 0; k < n_pair; k++) {
            flag = statistic_container << statistic_closure(group_i[k], group_j[k]);
        };

        return flag;
    };

    statistic_container.init_statistic(multcomp_update);

    if (!std::isnan(n_permu)) {
        if (n_permu == 0) {
            statistic_container.init_statistic_permu(n_permutation(group));

            do {
                multcomp_update();
            } while (next_permutation(group));
        } else {
            statistic_container.init_statistic_permu(n_permu);

            do {
                random_shuffle(group);
            } while (multcomp_update());
        }
    }

    return statistic_container.close();
}