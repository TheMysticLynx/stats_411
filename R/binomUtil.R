#' Calculate standard deviation for a binomial distribution DRV
#'
#'
#' @param n The number of trials
#' @param p the probability of success
#' @return The standard deviation of the DRV
#' @export
binom_sd <- function(x, prob) {

  sqrt(x*prob*(1-prob))

}


#' Calculate variance for a binomial distribution DRV
#'
#'
#' @param n The number of trials
#' @param p the probability of success
#' @return The variance of the DRV
#' @export
binom_var <- function(x, prob) {

  x*prob*(1-prob)
  
}