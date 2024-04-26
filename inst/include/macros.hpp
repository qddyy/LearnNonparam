#pragma once

#define FOR_EACH_1(macro, sth) macro(sth)
#define FOR_EACH_2(macro, sth, ...) macro(sth), FOR_EACH_1(macro, __VA_ARGS__)
#define FOR_EACH_3(macro, sth, ...) macro(sth), FOR_EACH_2(macro, __VA_ARGS__)
#define FOR_EACH_4(macro, sth, ...) macro(sth), FOR_EACH_3(macro, __VA_ARGS__)

#define SELECT_FOR_EACH(_1, _2, _3, _4, which, ...) which
#define FOR_EACH(macro, ...)                                                     \
    SELECT_FOR_EACH(__VA_ARGS__, FOR_EACH_4, FOR_EACH_3, FOR_EACH_2, FOR_EACH_1) \
    (macro, __VA_ARGS__)

#define CLONE(sth) clone(sth)
#define CLONE_EACH(...) FOR_EACH(CLONE, __VA_ARGS__)

#define PMT_PROGRESS_RETURN(impl, func_type, closure_type, ...)                                    \
    return (progress) ?                                                                            \
        impl<PermuBarShow, func_type, closure_type>(CLONE_EACH(__VA_ARGS__), statistic, n_permu) : \
        impl<PermuBarHide, func_type, closure_type>(CLONE_EACH(__VA_ARGS__), statistic, n_permu);

#define GENERATE_PMT_BODY(type, ...)                                                       \
    switch (TYPEOF(statistic_func)) {                                                      \
        case CLOSXP: {                                                                     \
            Function statistic(statistic_func);                                            \
            PMT_PROGRESS_RETURN(type##_pmt_impl, Function, Function, __VA_ARGS__)          \
        }                                                                                  \
        case EXTPTRSXP: {                                                                  \
            XPtr<type##_func> statistic_ptr(statistic_func);                               \
            type##_func statistic = *statistic_ptr;                                        \
            PMT_PROGRESS_RETURN(type##_pmt_impl, type##_func, type##_closure, __VA_ARGS__) \
        }                                                                                  \
        default:                                                                           \
            return R_NilValue;                                                             \
    }
