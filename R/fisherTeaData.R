# FISHER'S TEA TEST -------------------------------------------------------
#' Generate Fisher's Tea experiment
#'
#' @examples
#' fisherTeaData()
#' @export


fisherTeaData <- function() {
  m <- matrix(c(3, 1, 1, 3),
              nrow = 2,
              dimnames = list(Guess = c("Milk", "Tea"),
                              Truth = c("Milk", "Tea")))
  return(m)
}

