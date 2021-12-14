# GRAPH SIMULATED NORMAL DISTRIBUTIONS --------------------------------------------
#' Simulate normal distributions for the purpose of illustrating desirable properties of estimators
#' @param N Number of samples
#' @param mean Mean of population
#' @param std Standard deviation of population
#' @param seed Random number seed
#' @param biased Generate a biased sampling distribution? True or False
#' @param re Generate a relatively efficient sampling distribution? True or False
#' @param ic Generate an inconsistent sampling distribution? True or False
#' @examples
#' g <- myx(N = 10, biased = TRUE)
#' suppressWarnings(print(g$g.pop))
#' g$g.sampdist
#' myx(biased = FALSE, re = TRUE)$g.sampdist +
#' ggplot2::ggtitle("Relatively Efficient (N = 10)")
#' myx(biased = FALSE, re = FALSE)$g.sampdist +
#' ggplot2::ggtitle("Relatively Inefficient (N = 10)")
#' g <- myx(N = 100, biased = FALSE)
#' g$g.pop
#' myx(N = 5, biased = TRUE)$g.sampdist +
#' ggplot2::ggtitle("Cindy: Narrower, but biased so not consistent", "Small N")
#' myx(N = 100, biased = TRUE)$g.sampdist +
#' ggplot2::ggtitle("Cindy: Narrower, but biased so not consistent", "Large N")
#' @export
#'


# SIMULATE DISTS ------------------------------------------------------------

myx <- function(N = 5, mean = 100, std = 15, seed = 12345, biased = FALSE, re = TRUE, ic = FALSE) {


  if(biased) penalty <- 1.1
  else penalty <- 1

  if(re) sdpenalty <- 1
  else sdpenalty <- 2

  if(!ic) icpenalty <- 1
  else icpenalty <- 10


  set.seed(seed)
  v <- data.frame(x = rnorm(N, mean, std))

  set.seed(12345)
  p <- data.frame(x = soc429Code::rnorm_fixed(5000, mean, std))
  sampdist <- data.frame(x = soc429Code::rnorm_fixed(10000, mean, (std/sqrt(N))*sdpenalty*icpenalty))


  v$mean <- mean(v$x)
  v$mean[2:nrow(v)] <- NA
  p$mean <- mean(p$x)
  p$mean[2:nrow(p)] <- NA


  g.sample <- ggplot2::ggplot(data = v, ggplot2::aes(x)) +
    ggplot2::geom_dotplot(fill = "grey", method = "dotdensity", dotsize = 1, stackratio = 1) +
    ggplot2::xlab("Sample") +
    ggplot2:: theme(axis.text.y = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          plot.background = ggplot2::element_blank()) +
    ggplot2::ylab("") +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::scale_y_continuous(expand = c(0 , 0)) +
    ggplot2::geom_dotplot(ggplot2::aes(mean*penalty), fill = "red") +
    ggplot2::scale_x_continuous(limits = c(mean-3*std, mean+3*std),
                       breaks = seq(mean-3*std, mean+3*std, std)) +
    ggplot2::theme_minimal()

  g.pop <- ggplot2::ggplot(data = p, ggplot2::aes(x)) +
    ggplot2::geom_dotplot(fill = "grey", dotsize = 0.8, binwidth = 1, method = "dotdensity", stackratio = 0.5) +
    ggplot2::xlab("Population") + ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                               panel.grid.major=ggplot2::element_blank(),
                               panel.grid.minor=ggplot2::element_blank(),
                               plot.background=ggplot2::element_blank()) +
    ggplot2::ylab("") + ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2:: geom_dotplot(ggplot2::aes(mean), fill = "blue")  +
    ggplot2::scale_x_continuous(limits = c(mean-3*std, mean+3*std), breaks = seq(mean-3*std, mean+3*std, std)) +
    ggplot2::theme_minimal()

  g.sampdist <- ggplot2::ggplot(data = sampdist, ggplot2::aes(x*penalty)) +
    ggplot2:: geom_density(color = "red", fill = "red", alpha = 0.3) +
    ggplot2::xlab("Theoretical Sampling Distribution") +
    ggplot2:: theme(axis.text.y = ggplot2::element_blank(),
          panel.grid.major=ggplot2::element_blank(),
          panel.grid.minor=ggplot2::element_blank(),
          plot.background=ggplot2::element_blank()) +
    ggplot2:: geom_vline(xintercept = mean(sampdist$x*penalty), color = "red") +
    ggplot2:: ylab("") + ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::scale_x_continuous(limits = c(mean-3*std, mean+3*std),
                       breaks = seq(mean-3*std, mean+3*std, std)) + ggplot2::theme_minimal()


  return(list(mean = mean(v$x), sd = sd(v$x), g.pop = g.pop, g.sample = g.sample, g.sampdist = g.sampdist))

}


