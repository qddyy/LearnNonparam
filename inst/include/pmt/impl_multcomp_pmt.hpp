template <typename T, typename U>
NumericVector impl_multcomp_pmt(
    const IntegerVector group_i,
    const IntegerVector group_j,
    const NumericVector data,
    IntegerVector group,
    const U statistic_func,
    const R_xlen_t n_permu)
{
    T bar;

    R_len_t n_group = group[group.size() - 1];
    R_len_t n_pair = n_group * (n_group - 1) / 2;

    auto multcomp_update = [group_i, group_j, data, group, statistic_func, n_pair, &bar]() {
        auto statistic_closure = statistic_func(data, group);

        bool flag = false;
        for (R_len_t k = 0; k < n_pair; k++) {
            flag = bar << statistic_closure(group_i[k], group_j[k]);
        };

        return flag;
    };

    if (n_permu == 0) {
        bar.init(n_permutation(group), multcomp_update, n_pair);

        do {
            multcomp_update();
        } while (next_permutation(group));
    } else {
        bar.init(n_permu, multcomp_update, n_pair);

        do {
            random_shuffle(group);
        } while (multcomp_update());
    }

    return bar.close();
}