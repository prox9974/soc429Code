
# COMPARE CONTINUOUS ------------------

contCompare <- function(data, group, outcome, compare = FALSE, baseline = "", FUN = "mean", quantile = 0.5) {

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

    df <- data %>% dplyr::group_by(!!group)


    df %>% dplyr::summarise(FUN = switch(FUN,
                                          mean = mean(!!outcome, na.rm = TRUE),
                                          median = median(!!outcome, na.rm = TRUE),
                                          sd = sd(!!outcome, na.rm = TRUE),
                                          var = var(!!outcome, na.rm = TRUE),
                                          iqr = IQR(!!outcome, na.rm = TRUE),
                                          range = range(!!outcome, na.rm = TRUE),
                                          quantile = quantile(!!outcome, na.rm = TRUE, probs = quantile)
                                          )
                            )


    if(compare == TRUE) {
      df$b <- df[2][df[1] == baseline]
      df$diff <- FUN - df$b
      df$ratio <- FUN/df$b
      df$b <- NULL
     }

    print(df)
}

data(car)
library(magrittr)
head(GSSvocab)

out <- contCompare(data = GSSvocab, group = educGroup, outcome = vocab)
out
contCompare(data = GSSvocab, group = educGroup, outcome = vocab, FUN = "median")
contCompare(data = GSSvocab, group = educGroup, outcome = vocab, FUN = "iqr")
contCompare(data = GSSvocab, group = educGroup, outcome = vocab, FUN = "quantile", quantile = 0.5)
contCompare(data = GSSvocab, group = educGroup, outcome = vocab, FUN = "quantile", quantile = 0.99)

contCompare(data = GSSvocab, group = educGroup, outcome = vocab, compare = TRUE, baseline = "12 yrs")

