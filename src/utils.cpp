#include "utils.h"

std::tuple<NumericVector, ProgressBar> statistic_permu_with_bar(
    const unsigned n, const bool exact,
    const unsigned statistic_size)
{
    Environment base("package:base");
    Function interactive = base["interactive"];

    ProgressBar bar(n, as<bool>(interactive()));
    if (exact) {
        bar.set_label("Building exact permutation distribution");
    } else {
        bar.set_label("Sampling from exact permutation distribution");
    }

    NumericVector statistic_permu(no_init(n * statistic_size));
    if (statistic_size > 1) {
        statistic_permu.attr("dim") = IntegerVector::create(statistic_size, n);
    }

    return std::tuple<NumericVector, ProgressBar>(statistic_permu, bar);
}