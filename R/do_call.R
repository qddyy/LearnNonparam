# Updates the formals of a function with specified arguments before calling it.
# Enables non-standard evaluation by allowing expressions in certain arguments.
do_call <- function(func, default = list(), fixed = list(), ...) {
    # `formals()` returns a pairlist, not compatible with `list2env()`.
    args <- list2env(as.list.default(formals(func)), parent = emptyenv())

    params <- names(args)

    args <- list2env(envir = args, default)
    args <- list2env(envir = args, list(...))
    args <- list2env(envir = args, fixed)

    ...args <- setdiff(names(args), params)

    # `func` should be non-primitive.
    formals(func) <- lapply(
        `names<-`(params, params), function(param) {
            # `args[[param]]` might be a "missing symbol object".
            # Always use the full form `args[[param]]` here.
            # https://stackoverflow.com/questions/3892580
            if (!is.language(args[[param]])) {
                str2lang(paste0("parent.frame()$", param))
            } else args[[param]]
        }
    )

    # All arguments will be evaluated within the `args` environment.
    # https://stackoverflow.com/a/25371509/23137996
    eval(as.call(c(func, lapply(`names<-`(...args, ...args), as.name))), args)
}