#' @param scoring a character string specifying which scoring system to be used. 
#'
#' @param n_permu an integer specifying how many permutations should be used to construct the permutation distribution. If `NULL` (default) then all permutations are used. 
#' 
#' @param type a character string specifying the way to calculate the p-value. 
#' 
#' @param alternative a character string specifying the alternative hypothesis. 
#' @param null_value a number specifying the value of the parameter in the null hypothesis. 
#' @param conf_level a number specifying confidence level of the interval. 
#' @param correct a logical indicating whether to apply continuity correction in the normal approximation for the p-value. 
