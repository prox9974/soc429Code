# # PARTITION -------------------------------------------------------------
#' Partition a data frame into two pieces
#'
#' @param data A data frame
#' @param seed Random number seed
#' @param tprop Proportion for training data (between 0 and 1)
#' @examples
#' partition(mtcars, seed = 99999, tprop = 0.5)
#' @export
#'
partition <- function(data = df, seed = 12345, tprop = 0.2) {
  set.seed(seed)
  sample <- sample.int(n = nrow(data), size = floor(tprop*nrow(data)), replace = F)
  return(list("training" = data[sample, ], "validate" = data[-sample, ]))
}
