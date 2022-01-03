# ggBivTable ----------------------------------------

#' Graph a table of relative conditional frequencies as odds or proportions
#'
#' @param table bivTable object. Note that this is NOT a data frame
#' @param type "proportion" or "odds"
#' @param failures Total number of failures (for odds only)
#' @param success The level of the outcome variable that is a success
#' @importFrom magrittr %>%
#' @examples
#' bt <- bivTable(data = carData::GSSvocab, group = ageGroup, outcome = nativeBorn)
#' ggBivTable(table = bt)
#' ggBivTable(table = bt, type = "odds", success = "no", failures = 20)
#' @export

ggBivTable <- function(table, type = "proportion",  failures = 100, success = "") {

  df <- as.data.frame(table)

  if(type == "proportion") {

     g <- ggplot2::ggplot(df, ggplot2::aes_string(x = colnames(df)[1],
                                y = colnames(df)[4],
                 fill = colnames(df)[2])) +
       ggplot2::geom_bar(position = "stack", stat = "identity", color = "black") +
       ggplot2::coord_flip()

    }

  if(type == "odds") {

    if(success == "") {
      print("Please supply a success category for odds")
      return()
    }

    df <- df[df[2] == success, ]

    graphtitle <- paste0("Relative Number of Successes with ", failures, " Failures")

    df$Odds <- df$Odds*failures

    g <- ggplot2::ggplot(df, ggplot2::aes_string(x = colnames(df)[1],
                               y = "Odds")) +
      ggplot2::geom_bar(stat = "identity", alpha = 0.75, color = "black") +
      ggplot2::ylab("Successes") +
      ggplot2::ggtitle(paste0("Number of successes for every ", failures, " failures")) +
      ggplot2::coord_flip()


  }

  print(g)

}


