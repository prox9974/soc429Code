# GRAPH LOGTRANSFORMED OUTCOMES ---------------------------------------
#' Graph an outcome variable that has been log transformed
#' @param xstart starting value of x
#' @param xincrement how much x increases
#' @param int intercept
#' @param b1 slope
#' @param xlog whether x is logged or not
#' @param ylog whether y is logged or not
#' @param xmin minimum of x axis
#' @param xmax maximum of x axis
#' @param ymin minimum of y axis
#' @param ymax maximum of y axis
#' @param digits number of digits to print
#' @examples
#' library(carData)
#' m <- lm(data = USArrests, log(Rape) ~ Assault)
#' texreg::screenreg(m, digits = 3)
#' int <- m$coefficients["(Intercept)"]
#' b1 <- m$coefficients["Assault"]
#' logPlot(xstart = 150, xincrement = 1, int = int, b1 = b1, ylog = TRUE, xmin = 148, xmax = 152)
#' logPlot(xstart = 148, xincrement = 1, int = int, b1 = b1, ylog = TRUE, xmin = 140, xmax = 152)
#' logPlot(xstart = 148, xincrement = 100, int = int, b1 = b1, ylog = TRUE, xmin = 0, xmax = 400)
#' @export
#'
#'
# LOGPLOT -----------------------------------------------------------------

logPlot <- function(xstart = 0, xincrement = 1, int = 0, b1 = 1, xlog = FALSE,
                    ylog = TRUE, xmin, xmax, ymin = NULL, ymax = NULL, digits = 2) {

  if(ylog == TRUE & xlog == FALSE) {
    thisfun <- function(x) { exp(int + b1*x) }
    title <- "Logged Outcome, Non-Logged X"
    xend <- xstart + xincrement
  }
  else if(ylog == FALSE & xlog == FALSE) {
    thisfun <- function(x) { int + b1*x }
    title <- "Non-Logged Outcome & Non-Logged X"
    xend <- xstart + xincrement

  }
  else if(ylog == TRUE & xlog == TRUE) {
    thisfun <- function(x) exp(int + b1*log(x))
    title <- "Logged Outcome, Logged X"
    xend <- xstart*xincrement

  }
  else if(ylog == FALSE & xlog == TRUE) {
    thisfun <- function(x)  int + b1*log(x)
    title <- "Non-Logged Outcome & Logged X"
    xend <- xstart*xincrement

  }

  if(is.null(ymin) & is.null(ymax)) ylims <- c(NA, NA)
  else if(is.null(ymin) & !is.null(ymax)) ylims <- c(NA, ymax)
  else if(!is.null(ymin) & is.null(ymax)) ylims <- c(ymin, NA)
  else ylims <- c(ymin, ymax)

  g <- ggplot2::ggplot(data = data.frame(x = c(xmin, xmax)), ggplot2::aes(x = x)) +
    ggplot2::stat_function(fun = thisfun, size = 2) +
    ggplot2::theme_minimal() + ggplot2::xlab("X") + ggplot2::ylab("Y on its natural scale") +
    ggplot2::ggtitle(title) +
    ggplot2::scale_y_continuous(limits = ylims, breaks = scales::pretty_breaks(n = 10)) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    ggplot2::geom_hline(yintercept = thisfun(xstart), linetype = "dotted") +
    ggplot2::geom_hline(yintercept = thisfun(xend), linetype = "dotted") +
    ggplot2::geom_vline(xintercept = xstart, linetype = "dotted") +
    ggplot2::geom_vline(xintercept = xend, linetype = "dotted") +
    ggplot2::annotate(geom = "label", xmin, thisfun(xstart), label = paste("Start: ",round(thisfun(xstart), digits)), fill = "plum1", size = 6, hjust = 0) +
    ggplot2::annotate(geom = "label", xmin, thisfun(xend), label = paste("End: ", round(thisfun(xend), digits)), fill = "plum1", size = 6, hjust = 0)

  print(g)
}
