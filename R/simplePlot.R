#' simplePlot
#'
#' Generate plots for a continuous/quantitative univariate variable using the ggplot2 package
#' @param data The data frame that the variable is in
#' @param xcol The variable of interest, entered as a string
#' @param fill The fill color of the plots, entered as a string
#' @param color The outline color of the plots, entered as a string
#' @param bins The number of bins for the plots, entered as a numeric value
#' @param H Boolean operator to create histogram, default is FALSE
#' @param D Boolean operator to create density plot, default is FALSE
#' @param B Boolean operator to create boxplot, default is FALSE
#' @param P Boolean operator to create freqpoly plot, default is FALSE
#' @examples
#' simplePlot(mtcars, "wt")
#' simplePlot(mtcars, "wt", H = FALSE, B = FALSE, P = FALSE, color = "purple", fill = "pink")
#' @return The requested plots in ggplot2 format
#' @export
simplePlot <- function(data, xcol, fill = "white", color = "black", bins = 5,
                         H = TRUE,
                         D = TRUE,
                         B = TRUE,
                         P = TRUE) {

  # Check for ggplot2

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" must be installed to use this function.",
      call. = FALSE
    )
  }

  # Histogram

  if (H == TRUE) {
    plot1 <- ggplot2::ggplot(data = data,
                             ggplot2::aes_string(x = xcol)) +
      ggplot2::geom_histogram(fill = fill,
                              color = color,
                              bins = bins)
    print(plot1)

  }


  # Density

  if (D == TRUE) {

    plot2 <- ggplot2::ggplot(data = data,
                             ggplot2::aes_string(x = xcol)) +
      ggplot2::geom_density(fill = fill,
                            color = color)

    print(plot2)

  }

  # Freqpoly

  if (P == TRUE) {

    plot3 <- ggplot2::ggplot(data = data,
                             ggplot2::aes_string(x = xcol)) +
      ggplot2::geom_freqpoly(color = fill,
                             bins = bins)

    print(plot3)

  }

  # Boxplot

  if (B == TRUE) {

    plot4 <- ggplot2::ggplot(data = data,
                             ggplot2::aes_string(x = 0, y = xcol)) +
      ggplot2::geom_boxplot(fill = fill,
                            color = color) +
      ggplot2::xlab(" ") +
      ggplot2::ylab(xcol)

    print(plot4)

  }

}
