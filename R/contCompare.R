# COMPARE CONDITIONAL DISTRIBUTIONS WITH CONTINUOUS OUTCOME--------------------------------------------
#' Compare conditional distributions with continuous outcome with optional graph
#' @param data data frame
#' @param group your x variable (a factor variable)
#' @param outcome your continuous y variable
#' @param compare do you want to compare conditional outcomes to a baseline level of x TRUE or FALSE
#' @param baseline the omitted/baseline/reference level of x
#' @param FUN a function describing an outcome c(mean, median, sd, var, iqr, range)
#' @param quantile a vector to pass to quantile()
#' @param graph a TRUE/FALSE indicator of whether or not to print a graph
#' @param graphType if a graph will be plotted, print differences or ratios
#' @examples
#' library(car)
#' library(tidyverse)
#' contCompare(data = GSSvocab, group = educGroup, outcome = vocab, graph = FALSE)
#' contCompare(data = GSSvocab, group = educGroup, outcome = vocab, graph = TRUE,
#' graphType = "diff", compare = TRUE, baseline = "12 yrs")
#' contCompare(data = GSSvocab, group = educGroup, outcome = vocab, graph = TRUE,
#' graphType = "ratio", compare = TRUE, baseline = "16 yrs")
#' contCompare(data = GSSvocab, group = nativeBorn, outcome = age,
#' compare = FALSE, graph = TRUE, FUN = "median")
#' @export
#'

# COMPARE CONTINUOUS ------------------

contCompare <- function(data, group, outcome, compare = FALSE, baseline = "", FUN = "mean", quantile = 0.5,
                        graph = FALSE, graphType = "diff") {

    if(compare == TRUE & baseline == "") {
      print("Please supply a baseline row of X (the explanatory/row variable) for comparisons")
      return()
    }

    if(!methods::hasArg(data)|!methods::hasArg(group)|!methods::hasArg(outcome)) {
      print("Please supply a data frame, group, and outcome.")
      return()
    }


    group <- dplyr::enquo(group)
    outcome <- dplyr::enquo(outcome)



    if(compare == TRUE & baseline == "") {
      print("Please supply a baseline for comparisons.")
      return()
    }

    df <- data %>%
      dplyr::select(!!group, !!outcome) %>%
      dplyr::group_by(!!group)


    df <- df %>% dplyr::summarise(FUN = switch(FUN,
                                          mean = mean(!!outcome, na.rm = TRUE),
                                          median = median(!!outcome, na.rm = TRUE),
                                          min = min(!!outcome, na.rm = TRUE),
                                          max = max(!!outcome, na.rm = TRUE),
                                          sd = sd(!!outcome, na.rm = TRUE),
                                          var = var(!!outcome, na.rm = TRUE),
                                          iqr = IQR(!!outcome, na.rm = TRUE),
                                          quantile = quantile(!!outcome, na.rm = TRUE, probs = quantile)
                                          )) %>%
              tidyr::drop_na()


    if(compare == TRUE) {
      df$b <- df[[2]][df[[1]] == baseline]
      df$diff <- df$FUN - df$b
      df$ratio <- df$FUN/df$b
      df$b <- NULL
      df$g <- "Baseline"

      if(graph == TRUE & graphType == "diff") df$g <- ifelse(df$diff > 0, "Bigger",
                                                                 ifelse(df$diff < 0, "Smaller", "Baseline"))

      if(graph == TRUE & graphType == "ratio") df$g <- ifelse(df$ratio > 1, "Bigger",
                                                             ifelse(df$ratio < 1, "Smaller", "Baseline"))

      df$g[df[[1]] == baseline] <- "Baseline"
      df$g <- factor(df$g)

    }

    if(graph == TRUE & compare == FALSE) {

      g <- ggplot2::ggplot(data = df, ggplot2::aes_string(x = colnames(df)[1], y = colnames(df)[2])) +
      ggplot2::geom_bar(stat = "identity", alpha = 0.6, color = "black") +
      ggplot2::ylab(FUN) +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(10)) +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal()

      print(g)


    }

    if(graph == TRUE & compare == TRUE) {

      if(graphType == "diff") {

        g <- ggplot2::ggplot(data = df, ggplot2::aes_string(x = colnames(df)[1],
                                                            y = colnames(df)[3],
                                                            fill = "g")) +
          ggplot2::geom_bar(stat = "identity", color = "black") +
          ggplot2::scale_fill_manual(labels = c("Smaller", "Baseline", "Bigger"),
                                     values = c("Smaller" = "red", "Baseline" = "white", "Bigger" = "green")) +
          ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(10)) +
          ggplot2:: ylab("Difference") +
          ggplot2::coord_flip() +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = "none")
      }



      if(graphType == "ratio") {

        g <- ggplot2::ggplot(data = df, ggplot2::aes_string(x = colnames(df)[1],
                                                            y = colnames(df)[4],
                                                            fill = "g")) +
          ggplot2::geom_bar(stat = "identity", color = "black") +
          ggplot2::scale_fill_manual(labels = c("Smaller", "Baseline", "Bigger"),
                                     values = c("Smaller" = "red", "Baseline" = "white", "Bigger" = "green")) +
          ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(10)) +
          ggplot2:: ylab("Ratio") +
          ggplot2::coord_flip() +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = "none")

        }

    }
    if(compare == TRUE) df$g <- NULL
    if(graph == TRUE) print(g)


    return(df)


}





