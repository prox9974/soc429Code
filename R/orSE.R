# ODDS RATIO STANDARD ERROR -----------------------------------------------
#' Calculate the standard error for an odds ratio by exponentiating the log
#' @importFrom magrittr %>%
#' @param model a GLM model with family = "binomial"
#' @examples
#' m <- glm(data = mtcars, factor(am) ~ mpg + disp, family = "binomial")
#' summary(m)
#' orSE(m)
#' @export


orSE <- function(model) {
  broom::tidy(model) %>%
    dplyr::mutate(or = exp(estimate),
           var.diag = diag(vcov(model)),
           or.se = sqrt(or^2 * var.diag)) %>%
    dplyr::select(or.se) %>% unlist() %>% unname()
}


