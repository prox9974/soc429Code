# STANDARDIZED EXAMPLE ----------------------------------------------------
#' Regression with standardized variables
#' @param scale Whether variables are standardized or not
#' @examples
#' standardizedExample429()
#' @export
#'
#'

standardizedExample429 <- function(scale = FALSE) {
  set.seed(12345)
  x <- rnorm(100, 100, 15)
  e.bigsd <- rnorm(100, 0, 75)
  e.littlesd <- rnorm(100, 0, 2.5)

  y.big <- 10*x + e.bigsd
  y.little <- x + e.littlesd

  if(scale == TRUE) {
    y.big <- scale(y.big)
    y.little <- scale(y.little)
    x <- scale(x)
  }

  df <- data.frame(x, y.big, y.little)
  m.big <- lm(data = df, y.big ~ x)
  m.little <- lm(data = df, y.little ~ x)
  return(list(m.big = m.big, m.little = m.little, x = x, y.big = y.big, y.little = y.little))
}
