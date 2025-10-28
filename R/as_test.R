as_test <- function(class) {
    init_formals <- formals(class$public_methods$initialize)

    class_ <- R6Class(
        classname = paste0(class$classname, "_"),
        inherit = class,
        cloneable = FALSE,
        public = list(
            initialize = as.function.default(c(
                alist(args = ), init_formals,
                quote({
                    init <- if (is.null(names(args))) {
                        rep.int(FALSE, length(args))
                    } else {
                        names(args) %in% names(init_formals)
                    }

                    do.call(super$initialize, args[init])
                    do.call(super$test, args[!init])
                })
            )),

            test = NULL
        )
    )

    as.function.default(c(
        alist(... = ), init_formals,
        quote(class_$new(args = as.list.default(match.call())[-1]))
    ))
}