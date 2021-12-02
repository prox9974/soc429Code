# PRINT HELP TO CONSOLE ---------------------------------------------------

#' Print help file associated with a function
#'
#' @param topic Name of function
#' @param format Optional format
#' @param lines Optional lines to pull out
#' @return Printed output
#' @examples
#' printHelp(USArrests)
#' @export

# https://www.r-bloggers.com/printing-r-help-files-in-the-console-or-in-knitr-documents/
printHelp <- function(topic, format=c("text", "html", "latex", "Rd"),
                      lines=NULL) {

  format=match.arg(format)
  if (!is.character(topic)) topic <- deparse(substitute(topic))
  helpfile = utils:::.getHelpFile(help(topic))

  hs <- capture.output(switch(format,
                              text=tools::Rd2txt(helpfile),
                              html=tools::Rd2HTML(helpfile),
                              latex=tools::Rd2latex(helpfile),
                              Rd=tools::prepare_Rd(helpfile)
  )
  )

  if(!is.null(lines)) hs <- hs[lines]

  hs <- paste(unlist(hs), collapse = "~+")
  ps <- gregexpr("_\bS_\be_\be _\bA_\bl_\bs_\bo:", hs)
  hs <- substr(hs, 1, ps[[1]])
  hs <- gsub(pattern = "~+", replacement = "\n", x = hs)
  print(cat(hs))

}
