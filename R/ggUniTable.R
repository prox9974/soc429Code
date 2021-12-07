# ggUniTable ----------------------------------------

#' Graph a table of relative marginal frequencies as odds or proportions
#'
#' @param ut uniTable object. Note that this is NOT a data frame
#' @param type "proportion" or "odds"
#' @param total Total number of observations (for proportions only)
#' @param failures Total number of failures (for odds only)
#' @param success The level of the outcome variable that is a success
#' @importFrom magrittr %>%
#' @examples
#' ut <- uniTable(data = carData::Arrests, variable = employed)
#' ggUniTable(ut = ut, total = 600)
#' ggUniTable(ut = ut, type = "odds", success = "No", failures = 42)
#' @export

ggUniTable <- function(ut, type = "proportion", total = 100, failures = 100, success = "") {

  if(type == "odds" & success == "") {
    print("Please identify the 'success' category to calculate the odds.")
    return()
  }

  df <- as.data.frame(ut)
  vn <- colnames(df)[1]
  L <- levels(df[, 1])
  L.length <- length(L)

  if(type == "odds") {
    successes <- round(df$Odds[df[,1] == success]*failures, 0)
    print(successes)
    total <- successes + failures
  }


  graphtitle <- paste0("Relative Frequencies out of ", total, " Observations")

  # Create the ordered list of values

  gdf <- data.frame(norm = sort(round(runif(n = total), 3)))

  # Initialize data for loops

  i = 1
  seq <- vector()
  gp <- vector()
  gdf$seq <- ""
  startno <- 1

  # if(type == "proportion") {
  #   while(i <= L.length) {
  #     seq[i] <- round(df$Proportion[i]*total, 0)
  #     gdf$seq[startno:min(startno + seq[i], total)] <- L[i]
  #     startno <- min(seq[i] + 1, total)
  #     i <- i + 1
  #   }
  # }

  if(type == "proportion") {

    gdf <- df %>%
      dplyr::mutate(Frequency = round(Proportion*total, 0)) %>%
      dplyr::select(-Proportion, -Odds)



    gdf <- gdf[rep(1:nrow(gdf), gdf[["Frequency"]]), ]


    print(gdf)


    gdf <- gdf %>%
      dplyr::select(-Frequency) %>%
      dplyr::arrange(gdf[1]) %>%
      dplyr::rename(seq = names(gdf[1]))

    gdf$norm <- sort(runif(nrow(gdf)))

    print(gdf)
  }
  if(type == "odds") {
    seq <- unlist(c(rep(paste0("Failures (",failures,")"), failures),
                    rep(paste0(success," (", successes,")"), successes)))
    gdf$seq <- seq
  }

  g <- ggplot2::ggplot(data = gdf, ggplot2::aes(x = 1, y = norm, fill = seq)) +
    ggplot2::geom_dotplot(binwidth = 0.02, binaxis = 'y', stackdir = 'center') +
    ggplot2::coord_flip() + ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::theme_minimal() +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank()) +
    ggplot2::labs(fill = vn, position = "bottom") + ggplot2::ggtitle(graphtitle)
  print(g)

}


# library(car)
# t <- uniTable(data = Prestige %>% tidyr::drop_na(), variable = type)
# t
#
# ggUniTable(t, success = "wc")
# ggUniTable(t, success = "wc", type = "odds")
