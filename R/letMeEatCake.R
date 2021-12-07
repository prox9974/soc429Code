# letMeEatCake --------------------------------------------------------------------
#' Visualizing the idea of maximum likelihood with a silly cake example
#'
#' @examples
#' letMeEatCake()
#' @export

letMeEatCake <- function() {

  b2 <- -250000/47
  b1 <- 84500000/47
  max <- 151920213
  b0 <- 0
  myfun <- function(x) 10*(b0 + b1*x + b2*(x^2))/max
  g <- ggplot2::ggplot(data = data.frame(x = c(60, 280)), ggplot2::aes(x)) +
    ggplot2::stat_function(fun = myfun) +
    ggplot2::ylab("Quality") +
    ggplot2::xlab("Temperature") +
    ggplot2::geom_vline(xintercept = 169, linetype = "dotted") +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(10)) +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::annotate(geom = "label", x = c(75, 100, 169, 200, 230),
             y = c(myfun(75), myfun(100), myfun(169), myfun(200), myfun(230)),
             label = c("Gross", "Wet", "Perfect", "Overbaked", "Smoking")
    ) +
    ggplot2::theme_minimal()
  return(g)
}

