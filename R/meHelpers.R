#' Critical Value for a confidence level and n with t-dist
#'
#' @param n Integer. The sample size (must be greater than 0).
#' @param conf_level Numeric. The confidence level (must be > 0 and <= 1, e.g., 0.80 for 80% confidence).
#'
#' @return CV for the given input
#'
#' @export
cv_xbar <- function (n, conf_level) {
    df <- n - 1
    t_crit <- qt(1 - (1 - conf_level) / 2, df)
    return (t_crit)
}


#' Confidence Interval for the Mean using t-distribution
#'
#' Calculates the confidence interval for a population mean
#'
#' @param xbar Numeric. The sample mean.
#' @param sd Numeric. The sample standard deviation.
#' @param n Integer. The sample size (must be greater than 0).
#' @param conf_level Numeric. The confidence level (must be > 0 and <= 1, e.g., 0.80 for 80% confidence).
#'
#' @return A vector with the lower and upper bounds of the confidence interval.
#'
#' @export
ci_xbar <- function(xbar, sd, n, conf_level) {
  if (n <= 0) stop("Sample size 'n' must be greater than 0.")
  if (conf_level <= 0 || conf_level > 1) stop("Confidence level must be between 0 and 1.")
  
  df <- n - 1
  t_crit <- qt(1 - (1 - conf_level) / 2, df)
  margin_error <- t_crit * sd / sqrt(n)
  lowerbound <- xbar - margin_error
  upperbound <- xbar + margin_error
  return(c(lowerbound, upperbound))
}


#' Critical Value for a confidence level and n with t-dist
#'
#' @param n Integer. The sample size (must be greater than 0).
#' @param conf_level Numeric. The confidence level (must be > 0 and <= 1, e.g., 0.80 for 80% confidence).
#'
#' @return CV for the given input
#'
#' @export
cv_phat <- function(n, conf_level) {  
  z_crit <- qnorm(1 - (1 - conf_level) / 2)
  return(z_crit)
}


#' Confidence Interval for a Population Proportion
#'
#' Calculates the confidence interval for a population proportion using the normal approximation.
#'
#' @param phat Numeric. The sample proportion (e.g., 0.1 for 10%).
#' @param n Integer. The sample size.
#' @param conf_level Numeric. The confidence level (e.g., 0.98 for 98% confidence).
#'
#' @return A vector with the lower and upper bounds of the confidence interval.
#' 
#' @export
ci_phat <- function(phat, n, conf_level) {
  if (n <= 0) stop("Sample size 'n' must be greater than 0.")
  if (conf_level <= 0 || conf_level > 1) stop("Confidence level must be between 0 and 1.")
  
  z <- cv_phat(n, conf_level)


  se <- sqrt(phat * (1 - phat) / n)

  lower <- phat - z * se
  upper <- phat + z * se

  return(c(lower, upper))
}