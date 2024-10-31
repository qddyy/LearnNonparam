get_score <- function(x, scoring, n = length(x)) {
    rank <- rank(x)

    switch(scoring,
        rank = rank, vw = qnorm(rank / (n + 1)), expon = {
            expon <- cumsum(1 / n:1)
            remainder <- rank - floor(rank)
            expon[rank] * (1 - remainder) + expon[ceiling(rank)] * remainder
        }
    )
}