#' cv_sd
#'
#' Computes the critical values from the Chi-Square distribution for constructing a 
#' confidence interval for the population variance or standard deviation.
#'
#'
#' @param n Integer. The sample size (must be greater than 1).
#' @param alpha Numeric. 1 - the confidence level for the interval (must be between 0 and 1).
#'
#' @return A numeric vector of length 2, containing the lower and upper critical values 
#' from the Chi-Square distribution, in that order: \code{c(chi_lower, chi_upper)}.
#'
#' @examples
#' # Get 90% confidence interval critical values with a sample size of 11
#' cv_sd(n = 11, conf_level = 0.90)
#'
#' # Get 95% confidence interval critical values with a sample size of 30
#' cv_sd(n = 30, conf_level = 0.95)
#'
#' @export
cv_sd <- function(n, alpha) {
    if (n <= 1) {
        stop("Sample size must be greater than 1.")
    }
    if (alpha <= 0 || alpha >= 1) {
        stop("alpha must be between 0 and 1.")
    }

    df <- n - 1

    # Chi-square critical values
    chi_upper <- qchisq(1 - alpha / 2, df)
    chi_lower <- qchisq(alpha / 2, df)

    return(c(chi_lower, chi_upper))
}


#' ci_sd
#'
#' Computes the confidence interval for the population standard deviation
#' based on a sample standard deviation, sample size, and confidence level.
#'
#' @param sd Numeric. The sample standard deviation.
#' @param n Integer. The sample size.
#' @param conf_level Numeric. The confidence level (e.g., 0.80, 0.95).
#'
#' @return A numeric vector of length 2 containing the lower and upper bounds of the confidence interval.
#' @examples
#' ci_sd(sd = 1.79, n = 83, conf_level = 0.80)
#' ci_sd(sd = 2.5, n = 30, conf_level = 0.95)
ci_sd <- function(sd, n, conf_level) {
  if (n <= 1) {
    stop("Sample size must be greater than 1.")
  }
  if (sd <= 0) {
    stop("Sample standard deviation must be greater than 0.")
  }
  if (conf_level <= 0 || conf_level >= 1) {
    stop("Confidence level must be between 0 and 1.")
  }

  df <- n - 1
  alpha <- 1 - conf_level

    # Critical values from the Chi-Square distribution
  chi_vals <- cv_sd(n, alpha)
  chi_lower <- chi_vals[2]
  chi_upper <- chi_vals[1]

  # Confidence interval for population standard deviation
  lower_bound <- sqrt((df * sd^2) / chi_lower)
  upper_bound <- sqrt((df * sd^2) / chi_upper)

  return(c(lower_bound, upper_bound))
}


#' ci_var
#'  
#' Computes the confidence interval for the population variance based on a sample variance,
#' sample size, and confidence level.
#' 
#' @param var Numeric. The sample variance.
#' @param n Integer. The sample size.   
#' @param conf_level Numeric. The confidence level (e.g., 0.80, 0.95).
#'  
#' @return A numeric vector of length 2 containing the lower and upper bounds of the confidence interval.
#' 
#' 
ci_var <- function (var, n, conf_level) {
  if (n <= 1) {
    stop("Sample size must be greater than 1.")
  }
  if (var <= 0) {
    stop("Sample variance must be greater than 0.")
  }
  if (conf_level <= 0 || conf_level >= 1) {
    stop("Confidence level must be between 0 and 1.")
  }

  df <- n - 1
  alpha <- 1 - conf_level

    # Critical values from the Chi-Square distribution
  chi_vals <- cv_sd(n, alpha)
  chi_lower <- chi_vals[1]
  chi_upper <- chi_vals[2]

  # Confidence interval for population variance
  lower_bound <- (df * var) / chi_upper
  upper_bound <- (df * var) / chi_lower

  return(c(lower_bound, upper_bound))
}
