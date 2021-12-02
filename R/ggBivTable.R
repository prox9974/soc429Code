# ggBivTable ----------------------------------------

#' Graph a table of relative conditional frequencies as odds or proportions
#'
#' @param table bivTable object. Note that this is NOT a data frame
#' @param type "proportion" or "odds"
#' @param total Total number of observations (for proportions only)
#' @param failures Total number of failures (for odds only)
#' @param success The level of the outcome variable that is a success
#' @importFrom magrittr %>%
#' @examples
#' bt <- bivTable(data = carData::GSSvocab, group = ageGroup, outcome = nativeBorn)
#' ggBivTable(table = bt, total = 30)
#' ggBivTable(table = bt, type = "odds", success = "no", failures = 20)
#' @export

ggBivTable <- function(table, type = "proportion", total = 1000, failures = 100, success = "") {

  df <- as.data.frame(table)
  print(df)

  if(type == "proportion") {

    graphtitle <- paste0("Relative Number of Successes with a Total of ", total, " Events")

    df$sv <- -1
    df$sv[df[2] != levels(df[[2]])[2]] <- 1

    df <- df %>% dplyr::mutate(Frequency = round(Proportion*total,0)) %>%
      dplyr::select(-Proportion)

    df <- df[rep(1:nrow(df), df[["Frequency"]]), ] %>%
    dplyr::select(-Frequency)

    df <- df %>% dplyr::arrange(df[1], df[2])



    df <- df %>% dplyr::group_by(df[1]) %>%
      dplyr::mutate(sesq = round(sv*runif(dplyr::n()), digits = 5)) %>% dplyr::select(-sv)

    g <- ggplot2::ggplot(data = df, ggplot2::aes_string(x = colnames(df)[1],
                                                        y = colnames(df)[4],
                                                        fill = colnames(df)[2])) +
      ggplot2::geom_dotplot(binaxis = 'y', stackdir = 'center') +
      ggplot2::coord_flip() + ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank()) +
    ggplot2::ggtitle(graphtitle)
    print(g)

    }

  if(type == "odds") {

    graphtitle <- paste0("Relative Number of Successes with ", failures, " Failures")

    df$seq <- -1
    df$seq[df[2] == success] <- 1

    df$N <- round(df$Odds*failures, 0)
    df$N[df[2] != success] <- failures
    df <- df %>% dplyr::select(-Proportion, -Frequency)
    df <- df[rep(1:nrow(df), df[["N"]]), ]
    df$seq <- df$seq*runif(nrow(df))

    g <- ggplot2::ggplot(data = df, ggplot2::aes_string(x = colnames(df)[1],
                                                        y = colnames(df)[4],
                                                        fill = colnames(df)[2])) +
      ggplot2::geom_dotplot(binaxis = 'y', stackdir = 'center') +
      ggplot2::coord_flip() + ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank()) +
     ggplot2::labs(position = "bottom") + ggplot2::ggtitle(graphtitle)
    print(g)

  }

}


