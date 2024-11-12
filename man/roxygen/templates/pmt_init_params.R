#' @param type a character string specifying the way to calculate the p-value.
#' @param scoring a character string specifying the scoring system.
#' @param alternative a character string specifying the alternative hypothesis.
#' @param conf_level a number between zero and one indicating the confidence level to use.
#' @param n_permu an integer indicating number of permutations for the permutation distribution. If set to `0`, all permutations will be used.
#' @param correct a logical indicating whether to apply continuity correction in the normal approximation for the p-value.