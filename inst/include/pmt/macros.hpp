#pragma once

#define FOR_EACH_1(macro, sth) macro(sth)
#define FOR_EACH_2(macro, sth, ...) macro(sth), FOR_EACH_1(macro, __VA_ARGS__)
#define FOR_EACH_3(macro, sth, ...) macro(sth), FOR_EACH_2(macro, __VA_ARGS__)
#define FOR_EACH_4(macro, sth, ...) macro(sth), FOR_EACH_3(macro, __VA_ARGS__)

#define SELECT_FOR_EACH(_1, _2, _3, _4, which, ...) which
#define FOR_EACH(macro, ...)                                                     \
    SELECT_FOR_EACH(__VA_ARGS__, FOR_EACH_4, FOR_EACH_3, FOR_EACH_2, FOR_EACH_1) \
    (macro, __VA_ARGS__)

#define CLONE_EACH(...) FOR_EACH(clone, __VA_ARGS__)

#define PMT_PROGRESS_RETURN(impl, ...)                                         \
    return (progress) ?                                                        \
        impl<PermuBarShow>(CLONE_EACH(__VA_ARGS__), statistic_func, n_permu) : \
        impl<PermuBarHide>(CLONE_EACH(__VA_ARGS__), statistic_func, n_permu);
