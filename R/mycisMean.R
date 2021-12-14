# GRAPH SIMULATED SAMPLING DISTRIBUTION OF SAMPLE MEANS --------------------------------------------
#' Simulate sampling distributions of sample means
#' @param N Sample size
#' @param mean Mean of population
#' @param R Number of draws
#' @param std Standard deviation of population
#' @examples
#' mycisMean()
#' mycisMean(N = 100, R = 20)
#' @export
#'

mycisMean <- function( N = 5, R = 10, mean = 100, std = 15) {
  p <- data.frame(id = rep(1:R, each = N), x = rnorm(N*R, mean, std))
  s.m <- aggregate(p, FUN = mean, by = list(p$id))
  s.sd <- aggregate(p, FUN = var, by = list(p$id))

  s.m <- s.m[, c(1, 3)]
  s.sd <- s.sd[, c(1, 3)]

  s <- merge(s.m, s.sd, by = "Group.1")
  colnames(s) <- c("id", "mean", "sd")
  s$sd <- sqrt(s$sd)
  s$se <- s$sd/sqrt(N)
  s$ci.l <- s$mean + s$se*qt(p = 0.025, df = N - 1, lower.tail = TRUE)
  s$ci.u <- s$mean + s$se*qt(p = 0.025, df = N - 1, lower.tail = FALSE)

  s <- s[1:min(R, 50), ]

  g <- ggplot2::ggplot(data = s, ggplot2::aes(mean, id)) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci.l, xmax = ci.u), alpha = 0.5) +
    ggplot2::ylab("Sample #")  +
    ggplot2::geom_point(color = "red", size = 2) +
    ggplot2::scale_x_continuous(limits = c(mean - 3*std, mean + 3*std),
                       breaks = seq(mean-3*std, mean+3*std, std),
                       labels = seq(mean-3*std, mean+3*std, std) ) +
    ggplot2::ggtitle(paste("Mean Estimate when N = ", N)) +
    ggplot2::theme_minimal()

  print(g)
  invisible(list(p = p, s = s, g = g))
}
