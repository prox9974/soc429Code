
# CLEAR ALL ----------------

clearAll <- function() {
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  gc()
}
