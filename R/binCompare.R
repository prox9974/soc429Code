# COMPARE CONDITIONAL DISTRIBUTIONS WITH BINARY OUTCOME--------------------------------------------
#' Graph conditional statistics for a binary outcome and discrete X variable.
#' @param bt a bivTable object where compare = TRUE
#' @param success the level of the outcome you want to graph
#' @param type = rrd, rrr, or or
#' @examples
#' library(carData)
#' library(soc429Code)
#' bt <- bivTable(Greene, language, decision, compare = TRUE, baseline = "China")
#' binCompare(bt, success = "yes", type = "or")
#' binCompare(bt, success = "yes", type = "rrd")
#' @export
#'

# COMPARE CONTINUOUS ------------------

binCompare <- function(bt, success, type) {

      cn <- ifelse(type == "rrd", 7,
                   ifelse(type == "rrr", 6, 8))
      bt <- bt[bt[2] == success, c(1, cn)]

      if(type == "rrd") bt$g <- ifelse(bt[2] > 0, "Bigger",
                                       ifelse(bt[2] < 0, "Smaller", "Baseline"))

      if(type != "rrd") bt$g <- ifelse(bt[2] > 1, "Bigger",
                                       ifelse(bt[2] < 1, "Smaller", "Baseline"))

      mylab <- ifelse(type == "rrd", "Relative-Risk Difference",
                      ifelse(type == "rrr", "Relative-Risk Ratio", "Odds Ratio"))

      bt$g <- factor(bt$g)

        g <- ggplot2::ggplot(data = bt, ggplot2::aes_string(x = colnames(bt)[1],
                                                            y = colnames(bt)[2],
                                                            fill = "g")) +
          ggplot2::geom_bar(stat = "identity", color = "black") +
          ggplot2::scale_fill_manual(labels = c("Smaller", "Baseline", "Bigger"),
                                     values = c("Smaller" = "red", "Baseline" = "white", "Bigger" = "green")) +
          ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(10)) +
          ggplot2:: ylab(mylab) +
          ggplot2::coord_flip() +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = "none")

      print(g)

}





