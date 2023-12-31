get_last <- function(x) x[length(x)]

do_call <- function(func, default = NULL, fixed = NULL, ...) {
    env_args <- list2env(as.list(default))
    env_args <- list2env(list(...), envir = env_args)
    env_args <- list2env(as.list(fixed), envir = env_args)

    eval(
        as.call(c(func, sapply(names(env_args), as.name, simplify = FALSE))),
        envir = env_args, enclos = parent.frame()
    )
}

# for test()

deparse_1 <- function(expr) {
    paste(deparse(expr, width.cutoff = 500), collapse = " ")
}

get_data <- function(call, env) {
    data_exprs <- as.list(call)[-1]
    n_data <- length(data_exprs)

    if ((n_data == 1) & is.list(data_1 <- eval(data_exprs[[1]], envir = env))) {
        data_exprs <- data_1
    }

    data_names <- names(data_exprs)
    if (is.null(data_names)) {
        data_names <- rep_len("", n_data)
    }

    unlist(.mapply(
        dots = list(data_exprs, data_names),
        FUN = function(data, name) {
            `names<-`(
                list(eval(data, envir = env)),
                if (name != "") name else deparse_1(data)
            )
        }, MoreArgs = NULL
    ), recursive = FALSE, use.names = TRUE)
}

# for .calculate_score()

get_score <- function(x, method, n = length(x)) {
    rank <- rank(x)

    switch(method,
        rank = rank, vw = qnorm(rank / (n + 1)), expon = {
            expon <- cumsum(1 / n:1)
            remainder <- rank - floor(rank)
            expon[rank] * (1 - remainder) + expon[ceiling(rank)] * remainder
        }
    )
}

# for .calculate_p()

get_p_continous <- function(x, dist, side, ...) {
    F <- match.fun(paste0("p", dist))

    l <- F(x, ...)
    r <- 1 - l
    lr <- 2 * min(l, r)

    get(side)
}

get_p_decrete <- function(x, dist, side, ...) {
    F <- match.fun(paste0("p", dist))
    p <- match.fun(paste0("d", dist))

    l <- F(x, ...)
    r <- 1 - l + p(x, ...)
    lr <- 2 * min(l, r, 0.5)

    get(side)
}

#' @importFrom stats pbinom dbinom
get_p_binom <- function(x, n, p, side) {
    if (side == "lr") {
        if (p == 0) as.integer(x == 0) else if (p == 1) as.integer(x == n) else {
            d <- dbinom(x, n, p) * (1 + 1e-07)
            expect <- n * p
            if (x < expect) {
                y <- sum(dbinom(seq.int(ceiling(expect), n), n, p) <= d)
                pbinom(x, n, p) + pbinom(n - y, n, p, lower.tail = FALSE)
            } else if (x > expect) {
                y <- sum(dbinom(seq.int(0, floor(expect)), n, p) <= d)
                pbinom(y - 1, n, p) + pbinom(x - 1, n, p, lower.tail = FALSE)
            } else 1
        }
    } else {
        get_p_decrete(x, "binom", side, size = n, prob = p)
    }
}
