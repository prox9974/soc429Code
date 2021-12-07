# DURKHEIM ----------------------------------------------------------------
#' Generate Data from Durkheim's analysis of punishment & social organization
#' @param table simple v. complex outcome or dispersed v. concentrated outcome
#' @examples
#' durkheimData()
#' @export
#'


# Societal Complexity by Punishment Type, P. 623 Table 2 in Spitzer, S. (1974). Punishment and social organization: A study of Durkheim's theory of penal evolution. Law & Soc'y Rev., 9, 613.

durkheimData <- function(table = "complexity") {
  if(table == "complexity") {
    freq <- c(2, 4, 4, 7, 6, 1, 2, 4)
    puntype <- as.factor(rep(c(1:4), 2))
    soctype <- c(rep("Simple", 4), rep("Complex", 4))
  }
  else {
    freq <- c(3, 3, 8, 7, 9, 3, 1, 3)
    puntype <- as.factor(rep(c(1:4), 2))
    soctype <- c(rep("Dispersed", 4), rep("Concentrated", 4))
  }
  durkheim <- data.frame(soctype, puntype, freq)
  durkheim <- durkheim[rep(1:nrow(durkheim), durkheim[["freq"]]), ]
  durkheim["freq"] <- NULL
  return(durkheim)
}

# df <- durkheim()
# t <- table(df$soctype, df$puntype)
# t
# fisher.test(t)
# vcdExtra::GKgamma(t)

# df <- durkheim(type = "concentration")
# t <- table(df$soctype, df$puntype)
# t
# fisher.test(t)
# vcdExtra::GKgamma(t)
