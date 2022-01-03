# BIV TABLE ----------------------------------------

#' Create a table of conditional frequencies, relative frequencies, and odds for a single variable
#'
#' @param data A data frame
#' @param group Explanatory or row variable
#' @param outcome A dichotomous outcome
#' @param compare A logical variable indicating whether or not to compare conditional distributions
#' @param baseline A baseline level of the row/explanatory group; required if compare = TRUE
#' @examples
#' bivTable(data = carData::GSSvocab, group = ageGroup, outcome = nativeBorn)
#' @export
#'
bivTable <- function(data, group, outcome, compare = FALSE, baseline = "") {

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

  df <- data %>% dplyr::group_by(!!group, !!outcome) %>%
    dplyr::summarise(Frequency = dplyr::n()) %>%
    dplyr::mutate(Proportion = Frequency / sum(Frequency),
                  Odds = Frequency / (sum(Frequency) - Frequency))


 if(compare == TRUE) {

     df <- df %>% dplyr::group_by(!!outcome, !!group) %>%
       dplyr::mutate(order = ifelse(!!group == baseline, 0, 1)) %>%
         dplyr::ungroup() %>% dplyr::group_by(!!outcome) %>%
         dplyr::arrange(order, .by_group = TRUE) %>%
         dplyr::mutate(RRR = Proportion/Proportion[1],
                RRD = Proportion - Proportion[1],
                OR = Odds/Odds[1]) %>%
       dplyr::select(-order)

  }

  if(is.null(df)) return()
  else print(df, n = 100)

}


