#include "utils.h"

std::tuple<NumericVector, RObject> statistic_permu_with_bar(
    const unsigned n, const bool exact,
    const unsigned statistic_size)
{
    RObject bar = cli_progress_bar(n, NULL);
    cli_progress_set_type(bar, "iterator");

    if (exact) {
        cli_progress_set_name(bar, "Building exact permutation distribution");
    } else {
        cli_progress_set_name(bar, "Sampling from exact permutation distribution");
    }

    NumericVector statistic_permu(no_init(n * statistic_size));
    if (statistic_size > 1) {
        statistic_permu.attr("dim") = IntegerVector::create(statistic_size, n);
    }

    return std::tuple<NumericVector, RObject>(statistic_permu, bar);
}