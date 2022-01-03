# MINMAX (Quadratic) ------------------------------------------------------
#' Find the minimum or maximum of a quadratic function of x in a linear model
#'
#' @param M a linear model object created with lm
#' @param variable The name of the variable as a string
#' @examples
#' minMax(M = lm(data = mtcars, mpg ~ wt + I(wt^2)), variable = "wt")
#' @export
#'

minMax <- function(M, variable = "") {
  b1 <- attr(terms(M), "term.labels")[1]
  b2 <- attr(terms(M), "term.labels")[2]
  mm <- -1*coef(M)[b1]/(2*coef(M)[b2])
  return(mm)
}
