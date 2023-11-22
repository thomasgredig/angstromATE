#' ATE Recipe Status
#'
#' @description
#' Reads an XML Status file from the ATE thermal evaporator and returns
#' the procedural timeline.
#'
#' @param filename path and filename of XML Status file from Angstrom Thermal Evaporator
#'
#' @returns data frame with description steps, start and end times
#'
#' @author Thomas Gredig
#'
#' @importFrom XML xmlParse xmlToList
#'
#' @examples
#' fileName <- ATE.sampleFiles('_Status')
#' ATE.status(fileName)
#'
#' @export
ATE.status <- function(filename) {
  if (!(grepl('_Status_',filename) & grepl('xml$', filename))) {
    warning("File is not an ATE XML Status file.")
    return(NULL)
  }

  df <- xmlParse(filename)
  x <- xmlToList(df)
  x$RecipeElements -> xp
  q <- sapply(xp, unlist)
  # names(q[[1]])
  m <- lapply(q, function(x) { data.frame(desc = x['Description'],
                                          msg = x['MessageCode'],
                                          time.start = x['StartTime'],
                                          time.end = x['EndTime'])  } )
  do.call(rbind, m)
}
