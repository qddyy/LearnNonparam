last <- function(x) x[length(x)]

# for .feed

data_to_list <- function(...) {
    data <- list(...)

    if (length(data) == 1 & is.list(data[[1]])) {
        data <- data[[1]]
    }

    if (all(vapply(data, length, numeric(1)) >= 2)) data
}

# for .calculate_score

score <- function(x, method, n = length(x)) {
    rank <- rank(x)

    switch(method,
        rank = rank, vw = qnorm(rank / (n + 1)), expon = {
            expon <- cumsum(1 / n:1)
            remainder <- rank - floor(rank)
            expon[rank] * (1 - remainder) + expon[ceiling(rank)] * remainder
        }
    )
}

# for .calculate_p

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

# a progress bar (simple & fast)

#' @importFrom R6 R6Class
ProgressBar <- R6Class(
    classname = "ProgressBar",
    cloneable = FALSE,
    public = list(
        initialize = function(n) {
            private$.n_steps <- n
            private$.update_every <- ceiling(private$.n_steps / 100)
            private$.width <- getOption("width")
        },

        update = function() {
            private$.step <- private$.step + 1

            if (private$.step %% private$.update_every == 0) {
                percentage <- private$.step / private$.n_steps
                cat(
                    sprintf("\r %.0f%% >", percentage * 100),
                    strrep("=", private$.width * percentage)
                )
                flush.console()
            }
        }
    ),
    private = list(
        .step = 0,
        .n_steps = NULL,
        .update_every = NULL,
        .width = NULL
    )
)