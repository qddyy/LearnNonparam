# Updates the formals of a function with specified arguments before calling it.
# Enables non-standard evaluation by allowing expressions in certain arguments.
# Example: `do_call(func, list(args = bquote(.(constant_here) + symbol_here)))`
do_call <- function(func, default = list(), fixed = list(), ...) {
    # use `as.list()` because `formals()` returns a pairlist
    formals <- as.environment(as.list(formals(func)))

    formal_names <- names(formals)

    # use `base::list2env()` over `utils::modifyList()` for minimal dependency
    formals <- list2env(envir = formals, default)
    formals <- list2env(envir = formals, list(...))
    formals <- list2env(envir = formals, fixed)

    formals <- as.list.environment(formals, all.names = TRUE)[formal_names]

    # `func` should be non-primitive
    `formals<-`(func, value = formals)()
}