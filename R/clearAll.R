# CLEAR ALL ----------------
#' clear everything from Memory
#'
#' @examples
#' clearAll()
#' @export
#'
#'
clearAll <- function() {
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  gc()
}
