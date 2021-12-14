# GRAPH SIMULATED CONVIDENCE INTERVALS --------------------------------------------
#' Simulate normal distributions for the purpose of illustrating desirable properties of estimators
#' @param N Number of samples
#' @param mean Mean of population
#' @param std Standard deviation of population
#' @param R Number of replications
#' @examples
#' p1 <- mycisVar(N = 10, R = 10)
#' g1 <- p1$g + ggplot2::scale_x_continuous(limits = c(0, 1700))
#' p2 <- mycisVar(N = 50, R = 10)
#' g2 <- p2$g + ggplot2::scale_x_continuous(limits = c(0, 1700))
#' p3 <- mycisVar(N = 500, R = 10)
#' g3 <- p3$g + ggplot2::scale_x_continuous(limits = c(0, 1700))
#' gridExtra::grid.arrange(g1, g2, g3, nrow = 3)
#' @export
#'

mycisVar <- function( N = 5, R = 10, mean = 100, std = 15) {
  p <- data.frame(id = rep(1:R, each = N), x = rnorm(N*R, mean, std))
  s <- aggregate(p, FUN = var, by = list(p$id))
  s <- s[, c(1, 3)]
  colnames(s) <- c("id", "var")
  s$ci.l <- ((N - 1)*s$var)/qchisq(0.025, N - 1, lower.tail = TRUE)
  s$ci.u <- ((N - 1)*s$var)/qchisq(0.025, N - 1, lower.tail = FALSE)

  s <- s[1:min(R, 50), ]

  g <- ggplot2::ggplot(data = s, ggplot2::aes(var, id)) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci.l, xmax = ci.u), alpha = 0.5) +
    ggplot2::ylab("Sample #")  +
    ggplot2::geom_point(color = "red", size = 2) +
    ggplot2::ggtitle(paste("Variance Estimate when N = ", N)) +
    ggplot2::theme_minimal()
  return(list(p = p, s = s, g = g))
}



