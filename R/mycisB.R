# GRAPH SIMULATED SAMPLING DISTRIBUTION OF SAMPLE SLOPES & INTERCEPTS---------------------------------------
#' Graph simulated sampling distribution of sample slopes & intercepts
#' @param N Sample size
#' @param mean Mean of population
#' @param R Number of draws
#' @param std Standard deviation of population
#' @param intc Intercept value
#' @param slope Slope value
#' @examples
#' g <- mycisB()
#' g$g.int
#' g$g.slope
#' @export
#'
#'
mycisB <- function( N = 5, R = 25, mean = 100, std = 15, intc = -2, slope = 0.25) {
  p <- data.frame(id = rep(1:R, each = N), x = rnorm(N*R, mean, std))
  p$y <- intc +slope*p$x + rnorm(R*N, 0, 1)

  m <- lme4::lmList(formula = y ~ x | id, data = p)

  m.b0 <- data.frame(b = coef(m)["(Intercept)"],
                     type = "Intercept",
                     confint(m, "(Intercept)"))
  colnames(m.b0) <- c("b", "type", "ci.l", "ci.u")
  m.b0 <- m.b0[1:min(R, 50), ]

  m.b1 <- data.frame(b = coef(m)["x"],
                     type = "slope",
                     confint(m, "x"))
  colnames(m.b1) <- c("b", "type", "ci.l", "ci.u")
  m.b1 <- m.b1[1:min(R, 50), ]

  m.b0$id <- c(1:R)
  m.b1$id <- c(1:R)


  g.int <- ggplot2::ggplot(data = m.b0, ggplot2::aes(b, id))  +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci.l, xmax = ci.u), alpha = 0.5) +
    ggplot2::ylab("Sample #")  +
    ggplot2::geom_point(color = "red", size = 2)  +
    ggplot2::ggtitle(paste("Intercept Estimate when N = ", N)) +
    ggplot2::geom_vline(xintercept = intc, color = "skyblue3", linetype = "dashed") +
    ggplot2::theme_minimal()

  g.slope <- ggplot2::ggplot(data = m.b1, ggplot2::aes(b, id))  +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci.l, xmax = ci.u), alpha = 0.5) +
    ggplot2::ylab("Sample #")  +
    ggplot2::geom_point(color = "red", size = 2) +
    ggplot2::ggtitle(paste("Slope Estimate when N = ", N)) +
    ggplot2::geom_vline(xintercept = slope, color = "skyblue3", linetype = "dashed") +
    ggplot2::theme_minimal()

  invisible(list(m = m, g.int = g.int, g.slope = g.slope))
}
