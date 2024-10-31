do_call <- function(func, default = NULL, fixed = NULL, ...) {
    env_args <- list2env(as.list(default))
    env_args <- list2env(list(...), envir = env_args)
    env_args <- list2env(as.list(fixed), envir = env_args)

    args <- names(env_args)
    eval(
        as.call(c(func, lapply(`names<-`(args, args), as.name))),
        envir = env_args, enclos = parent.frame()
    )
}