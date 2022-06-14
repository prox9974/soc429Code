# ggUniTable ----------------------------------------

#' Graph a table of relative marginal frequencies as odds or proportions
#'
#' @param ut uniTable object. Note that this is NOT a data frame
#' @param type "proportion" or "odds"
#' @param failures Total number of failures (for odds only)
#' @param success The level of the outcome variable that is a success
#' @importFrom magrittr %>%
#' @examples
#' ut <- uniTable(data = carData::Arrests, variable = employed)
#' ggUniTable(ut = ut)
#' ggUniTable(ut = ut, type = "odds", success = "No", failures = 10)
#' @export

ggUniTable <- function(t, type = "proportion", failures = 100, success = "") {

  if(type == "odds" & success == "") {
    print("Please identify the 'success' category to calculate the odds.")
    return()
  }

  mytheme <- function(){
    theme <- ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
    )
    return(theme)}


  colnames(t)[1] <- "Level"

  if(type == "proportion") {

    g <- ggplot2::ggplot(data = t) +
      ggplot2::geom_col(ggplot2::aes_string(y = colnames(t)[3], x = 1, fill = colnames(t)[1])) +
      mytheme() + ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(10)) + ggplot2::coord_flip()
    print(g)
  }

  if(type == "frequency") {

    g <- ggplot2::ggplot(data = t) +
      ggplot2::geom_col(ggplot2::aes_string(y = colnames(t)[2], x = 1, fill = colnames(t)[1])) +
      mytheme() + ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(10)) + ggplot2::coord_flip()
    print(g)
  }

  if(type == "odds") {

    # Get the proportion that matches the successes level

    prop <- t[, 4][t[, 1] == success]

    df.s <- data.frame(Level = c("Successes", "Failures"),
                       Outcome = c(prop*failures, failures)
    )
    df.s
    df.s$Level <- factor(df.s$Level, levels = c("Successes", "Failures"), ordered = TRUE)

    ggplot2::ggplot(data = df.s) +
      ggplot2::geom_col(ggplot2::aes(y = Outcome, x = 1, fill = Level)) + mytheme() +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(10)) + ggplot2::coord_flip()



  }

}

