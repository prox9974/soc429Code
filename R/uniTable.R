# UNITABLE ---------------------------

#' Create a table of frequencies, relative frequencies, and odds for a single variable
#'
#' @param data A data frame
#' @param variable A variable
#' @importFrom magrittr %>%
#' @examples
#' library(carData)
#' uniTable(data = carData::Arrests, variable = employed)
#' @export

uniTable <- function(data, variable) {

  variable <- dplyr::enquo(variable)
  data %>% dplyr::group_by(!!variable) %>%
    dplyr::summarise(Frequency = dplyr::n()) %>%
    dplyr::mutate(Proportion = Frequency / sum(Frequency),
           Odds = Frequency / (sum(Frequency) - Frequency))

}



