#' ATE Deposition Summary
#'
#' @description
#' Reads an XML Status file from the ATE thermal evaporator and returns
#' the procedural timeline.
#'
#' @param filename path and filename of XML Status file from Angstrom Thermal Evaporator
#' @param summaryOnly logical, if \code{TRUE}, returns summary for one layer
#'
#' @returns data frame with description steps, start and end times
#'
#' @author Thomas Gredig
#'
#' @importFrom XML xmlParse xmlToList
#'
#' @examples
#' fileName <- ATE.sampleFiles('_Complete_')
#' ATE.complete(fileName)
#'
#' @export
ATE.complete <- function(filename, summaryOnly = FALSE) {
  if (!(grepl('_Complete_',filename) & grepl('xml$', filename))) {
    warning("File is not an ATE XML Complete file.")
    return(NULL)
  }

  df <- xmlParse(filename)
  x <- xmlToList(df)
  x$Layers -> xp
  q <- sapply(xp, unlist)

  df2 <- data.frame(
    name = names(df2) <- attr(q,'dimnames')[[1]],
    value = as.vector(q)
  )

  df2
}
