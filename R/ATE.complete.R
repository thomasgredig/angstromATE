#' ATE Deposition Summary
#'
#' Extracts information about the deposition thickness from the completed
#' status XML file at the end of the deposition.
#'
#' @description
#' Reads an XML Status file from the ATE thermal evaporator and returns
#' the procedural timeline.
#'
#' @param filename XML Status file from Angstrom Thermal Evaporator
#' @param summaryOnly logical, if \code{TRUE}, returns summary for one layer
#'
#' @returns deposition thickness, rate, ramping times, and actions
#'
#' @author Thomas Gredig
#'
#' @importFrom xml2 read_xml
#' @importFrom stringr str_match_all
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

  df <- xml2::read_xml(filename)
  x <- xml2_to_list(df)
  x$Layers -> xp
  q <- sapply(xp, unlist)

  df2 <- data.frame(
    name =  attr(q,'dimnames')[[1]],
    value = as.vector(q)
  )


  if (summaryOnly) {
    # extract dates and times from filename
    pattern <- "(20\\d{6})_(\\d{6})"
    date_matches <- str_match_all(filename, pattern)[[1]]

    if (nrow(date_matches)==1) { date_matches = rbind(date_matches, date_matches) }
    if (nrow(date_matches) != 2) {stop("AE complete file has no start date encoded.")}
    # only report a subset of that data:
    df2 <- data.frame(
      action = paste( df2$value[grepl("ActionName", df2$name)] , collapse = ', '),
      ramping = paste( df2$value[grepl("RampRate$", df2$name) | grepl("RampRateTimeUnitDefinition.UnitId.text", df2$name)], collapse = ', '),
      thickness = paste( df2$value[grepl("TargetThickness", df2$name)] , collapse = ', '),
      rate = paste( df2$value[grepl("TargetRate", df2$name)] , collapse = ', '),
      start.date = date_matches[1,2],
      start.time = date_matches[1,3],
      end.date = date_matches[2,2],
      end.time = date_matches[2,3]
    )
  }

  df2
}

xml2_to_list <- function(x) {
  kids <- xml2::xml_children(x)

  if (length(kids) == 0) {
    txt <- xml2::xml_text(x)
    return(txt)
  }

  out <- lapply(kids, xml2_to_list)
  names(out) <- xml2::xml_name(kids)

  attrs <- xml2::xml_attrs(x)
  if (length(attrs) > 0) {
    out$.attrs <- as.list(attrs)
  }

  out
}
