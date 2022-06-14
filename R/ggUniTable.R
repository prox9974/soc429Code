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

  # df <- as.data.frame(ut)
  # vn <- colnames(df)[1]
  # L <- levels(df[, 1])
  # L.length <- length(L)
  #
  # if(type == "odds") {
  #   successes <- round(df$Odds[df[,1] == success]*failures, 0)
  #   print(successes)
  #   total <- successes + failures
  # }
  #
  #
  # graphtitle <- paste0("Relative Frequencies out of ", total, " Observations")
  #
  # # Create the ordered list of values
  #
  # gdf <- data.frame(norm = sort(round(runif(n = total), 3)))
  #
  # # Initialize data for loops
  #
  # i = 1
  # seq <- vector()
  # gp <- vector()
  # gdf$seq <- ""
  # startno <- 1
  #
  # # if(type == "proportion") {
  # #   while(i <= L.length) {
  # #     seq[i] <- round(df$Proportion[i]*total, 0)
  # #     gdf$seq[startno:min(startno + seq[i], total)] <- L[i]
  # #     startno <- min(seq[i] + 1, total)
  # #     i <- i + 1
  # #   }
  # # }
  #
  # if(type == "proportion") {
  #
  #   gdf <- df %>%
  #     dplyr::mutate(Frequency = round(Proportion*total, 0)) %>%
  #     dplyr::select(-Proportion, -Odds)
  #
  #
  #
  #   gdf <- gdf[rep(1:nrow(gdf), gdf[["Frequency"]]), ]
  #
  #   gdf <- gdf %>%
  #     dplyr::select(-Frequency) %>%
  #     dplyr::arrange(gdf[1]) %>%
  #     dplyr::rename(seq = names(gdf[1]))
  #
  #   gdf$norm <- sort(runif(nrow(gdf)))
  #
  # }
  # if(type == "odds") {
  #   seq <- unlist(c(rep(paste0("Failures (",failures,")"), failures),
  #                   rep(paste0(success," (", successes,")"), successes)))
  #   gdf$seq <- seq
  # }
  #
  # print(gdf)
  #
  # g <- ggplot2::ggplot(data = gdf, ggplot2::aes(x = 1, y = norm, fill = seq)) +
  #   ggplot2::geom_col(binwidth = 0.02) +
  #   ggplot2::coord_flip() + ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::theme_minimal() +
  #   ggplot2::theme(axis.text.y = ggplot2::element_blank(),
  #                  axis.ticks.y = ggplot2::element_blank()) +
  #   ggplot2::labs(fill = vn, position = "bottom") +
  #   ggplot2::ggtitle(graphtitle)
  # print(g)


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

