#' Calculate z score given x, mean and std dev
#'
#'
#' @param x the given x
#' @param mean the mean of the dist
#' @param sd the standard deviation of the dist
#' @return the z score for the given inputs
#' @export
z_score <- function(x, mean, sd) {
  (x - mean) / sd
}


#' Calculate probability that sample proportion is greater or less than given value
#'
#' Uses the normal approximation to compute P(phat > phat_val) or P(phat < phat_val)
#'
#' @param p the population proportion
#' @param phat the sample proportion value to test against
#' @param n the sample size
#' @param lower.tail logical; if TRUE, returns P(phat < phat_val), otherwise P(phat > phat_val)
#' @return the probability that the sample proportion is on the specified tail of phat (rounded to 4 dig)
#' @export
prob_phat <- function(p, phat, n, lower.tail = TRUE) {
  se <- sqrt(p * (1 - p) / n)                    # Standard error AKA sd phat
  z <- z_score(phat, mean = p, sd = se)          
  prob <- pnorm(z, lower.tail = lower.tail)     

  return(round(prob,4))
}


#' Calculate probability that sample proportion is within or greater than a given margin of error
#'
#' Uses the normal approximation to compute P(phat > phat_val) or P(phat < phat_val) and find the difference or sum
#'
#' @param p the population proportion
#' @param me the margin of error
#' @param n the sample size
#' @param lower.tail logical; if TRUE, find the prob that phat is within the ME, otherwise
#'                    prob that phat is outside of it
#' @return the probability that the sample proportion is on the specified side of the ME (rounded to 4 dig)
#' @export
me_phat <- function(p, me, n, lower.tail = TRUE) {
  se <- sqrt(p * (1 - p) / n)

  z_upper <- z_score(p + me, mean = p, sd = se)
  z_lower <- z_score(p - me, mean = p, sd = se)

  if (lower.tail) {
    # Probability that phat is within margin (middle area)
    prob <- pnorm(z_upper, lower.tail = TRUE) - pnorm(z_lower, lower.tail = TRUE)
  } else {
    # Probability that phat is outside margin (two tails)
    prob <- pnorm(z_lower, lower.tail = TRUE) + pnorm(z_upper, lower.tail = FALSE)
  }

  return(round(prob, 4))
}


#' Calculate probability that sample mean is greater or less than a given value
#'
#' Uses the normal approximation based on the Central Limit Theorem to compute
#' P(sample mean > xbar) or P(sample mean < xbar)
#'
#' @param mu the population mean
#' @param sd the population standard deviation
#' @param xbar the sample mean value to test against
#' @param n the sample size
#' @param lower.tail logical; if TRUE, returns P(mean < xbar), otherwise P(mean > xbar)
#' @return the probability that the sample mean is on the specified tail, rounded to 4 digits
#' @export
prob_xbar <- function(mu, sd, xbar, n, lower.tail = TRUE) {
  se <- sd / sqrt(n)
  z <- z_score(xbar, mean = mu, sd = se)
  prob <- pnorm(z, lower.tail = lower.tail)
  return(round(prob, 4))
}


#' Calculate probability that sample mean is within or greater than a given margin of error
#'
#' Uses the normal approximation to compute P(mean > mu + me), P(mean < mu - me),
#' or the probability that the sample mean is within the margin.
#'
#' @param mu the population mean
#' @param sd the population standard deviation
#' @param me the margin of error
#' @param n the sample size
#' @param lower.tail logical; if TRUE, find the probability that the sample mean is within the ME,
#'                   otherwise probability that it is outside
#' @return the probability that the sample mean is on the specified side of the ME, rounded to 4 digits
#' @export
me_xbar <- function(mu, sd, me, n, lower.tail = TRUE) {
  se <- sd / sqrt(n)

  z_upper <- z_score(mu + me, mean = mu, sd = se)
  z_lower <- z_score(mu - me, mean = mu, sd = se)

  if (lower.tail) {
    # Probability that sample mean is within margin (middle area)
    prob <- pnorm(z_upper, lower.tail = TRUE) - pnorm(z_lower, lower.tail = TRUE)
  } else {
    # Probability that sample mean is outside margin (two tails)
    prob <- pnorm(z_lower, lower.tail = TRUE) + pnorm(z_upper, lower.tail = FALSE)
  }

  return(round(prob, 4))
}
