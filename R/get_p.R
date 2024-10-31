get_p_continous <- function(x, dist, side, ...) {
    F <- match.fun(paste0("p", dist))

    delayedAssign("l", F(x, ...))
    delayedAssign("r", 1 - l)
    delayedAssign("lr", 2 * pmin(l, r))

    eval(as.name(side))
}

get_p_decrete <- function(x, dist, side, ...) {
    F <- match.fun(paste0("p", dist))
    p <- match.fun(paste0("d", dist))

    delayedAssign("l", F(x, ...))
    delayedAssign("r", 1 - l + p(x, ...))
    delayedAssign("lr", 2 * pmin(l, r, 0.5))

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
    } else get_p_decrete(x, "binom", side, size = n, prob = p)
}