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
#' ATE.complete(fileName, TRUE)
#' ATE.complete(fileName)
#'
#' @export
ATE.complete <- function(filename, summaryOnly = FALSE) {
  if (!((grepl('_Complete_',filename) | grepl('_Original_',filename)) & grepl('xml$', filename))) {
    warning("File is not an ATE XML Complete file.")
    return(NULL)
  }

  df <- xmlParse(filename)
  x <- xmlToList(df)
  x$Layers -> xp
  q <- sapply(xp, unlist)

  df2 <- data.frame(
    name =  attr(q,'dimnames')[[1]],
    value = as.vector(q)
  )

  if (summaryOnly) {
    # only report a subset of that data:
    df2 <- data.frame(
      action = paste( df2$value[grepl("ActionName", df2$name)] , collapse = ', '),
      ramping = paste( df2$value[grepl("RampRate$", df2$name) | grepl("RampRateTimeUnitDefinition.UnitId.text", df2$name)], collapse = ', '),
      thickness = paste( df2$value[grepl("TargetThickness", df2$name)] , collapse = ', '),
      rate = paste( df2$value[grepl("TargetRate", df2$name)] , collapse = ', ')
    )
  }

  df2
}
