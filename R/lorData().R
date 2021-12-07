# LORD OF THE RINGS SIMULATED DATA ----------------------------------------
#' Simulated Lord of the Rings Data
#'
#' @examples
#' lorData()
#' @export
#'
#'
lorData <- function() {
  character <- c("Frodo", "Bilbo", "Gollum", "Sauron", "Samwise", "Isildur", "Tom", "Gandalf", "Aragorn", "Arwen", "Legolas", "Boromir", "Gimli", "Smaug")
  ring <- c(rep("No Ring", 7), rep("Ring", 7))
  ring.val <- c(rep(0, 7), rep(1, 7))
  set.seed(123456)
  e <- data.frame(e = rnorm(14, 0, 0.3))
  interest <- ring.val*runif(14, 1, 6) + rnorm(14)
  rings <- data.frame(character, ring, ring.val, interest)
  rings$corrupt <- 2 + 2*ring.val + 0.8*interest + 0.25*ring.val*interest + e$e
  return(rings[, -3])
}


lorData()
