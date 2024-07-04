#' Imports Angstrom Engineering Thermal Evaporator Log Data
#'
#' @param filename path and file name of CSV with ATE log file
#'
#' @author Thomas Gredig
#'
#' @examples
#' fileName = ATE.sampleFiles("csv")[1]
#' d = ATE.import(fileName)
#' head(d)
#'
#' @importFrom utils read.csv
#' @export
ATE.import <- function(filename) {
  # checks for file validity
  if (!file.exists(filename)) stop("ATE log file is not found.")

  # load the log file, need to change one column
  d = read.csv(filename, row.names = NULL)
  names(d) = c(names(d)[-1],"empty")
  d$empty<-NULL

  # clean up some columns in the file
  if (!(d$Version[1] == "5.0.10.10b1b9a6b0")) stop("ATE log file has version that cannot be processed.")
  d$Version <- NULL

  # Find TRUE/FALSE columns
  m1 <- c(which(d[1,]=="False"),which(d[1,]=="True"))
  d[,m1] <- (d[,m1]=="True")

  d
}

