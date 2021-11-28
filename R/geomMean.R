# GEOMETRIC MEAN ---------------------------

#' Take the geometric mean of a vector of numbers
#'
#' @param x A vector of numbers
#' @param na.rm A logical scalar. Should missing values (including NaN)
#'   be removed? Default is TRUE
#' @return The geometric mean
#' @examples
#' geomMean(c(1, 2, 3, 4, 5, 6), na.rm = TRUE)
#' @export

geomMean <- function(x, na.rm = TRUE) {
  exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
}
