# RNORM WITH FIXED PARAMETERS ---------------------------------------------
#' Create a normal random variable of a given length with fixed mean and standard deviation
#'
#' @param n Number of observations
#' @param mu mean
#' @param sigma standard deviation
#' @examples
#' rnorm_fixed(100, mu = 0, sigma = 1)
#' @export
#'

rnorm_fixed = function(n, mu=0, sigma=1) {
  x = rnorm(n)  # from standard normal distribution
  x = sigma * x / sd(x)  # scale to desired SD
  x = x - mean(x) + mu  # center around desired mean
  return(x)
}
