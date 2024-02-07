# for test()

get_data <- function(call, env) {
    data_exprs <- as.list(call)[-1]
    n_data <- length(data_exprs)

    if (n_data == 1 & is.list(data_1 <- eval(data_exprs[[1]], envir = env))) {
        data_exprs <- data_1
        n_data <- length(data_1)
    }

    data_names <- names(data_exprs)
    if (is.null(data_names)) {
        data_names <- rep.int("", n_data)
    }

    `names<-`(lapply(
        seq_len(n_data), function(i) {
            if (data_names[[i]] == "") {
                data_names[[i]] <<- paste(
                    deparse(data_exprs[[i]], width.cutoff = 500), collapse = " "
                )
            }

            data_i <- eval(data_exprs[[i]], envir = env)
            if (!is.numeric(data_i)) {
                stop("Data vector ", i, " is not numeric")
            } else if (anyNA(data_i)) {
                stop("Data vector ", i, " contains NA")
            } else data_i
        }
    ), data_names)
}

# for plot()

do_call <- function(func, default = NULL, fixed = NULL, ...) {
    env_args <- list2env(as.list(default))
    env_args <- list2env(list(...), envir = env_args)
    env_args <- list2env(as.list(fixed), envir = env_args)

    eval(
        as.call(c(func, sapply(names(env_args), as.name, simplify = FALSE))),
        envir = env_args, enclos = parent.frame()
    )
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
    lr <- 2 * pmin(l, r)

    eval(as.name(side))
}

get_p_decrete <- function(x, dist, side, ...) {
    F <- match.fun(paste0("p", dist))
    p <- match.fun(paste0("d", dist))

    l <- F(x, ...)
    r <- 1 - l + p(x, ...)
    lr <- 2 * pmin(l, r, 0.5)

    eval(as.name(side))
}

#' @importFrom stats pbinom dbinom
get_p_binom <- function(x, n, p, side) {
    if (side == "lr") {
        if (p == 0) {
            as.integer(x == 0)
        } else if (p == 1) {
            as.integer(x == n)
        } else {
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