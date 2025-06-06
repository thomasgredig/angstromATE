#' Imports Angstrom Engineering Thermal Evaporator Log Data
#'
#' @param filename CSV filename including path for the ATE log file
#'
#' @author Thomas Gredig
#'
#' @examples
#' fileName = ATE.sampleFiles("csv")[1]
#' d = ATE.import(fileName)
#' head(d)
#'
#' @return data frame with around 50 variables and rows that represent time;
#'       the variables include Date, Time, SubstrateShutterOpen, ChamberPressure
#'       and many other parameters.
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
  if (!(d$Version[1] == "5.0.10.10b1b9a6b0")) warning("ATE log file non standard version.")
  d$Version <- NULL

  # Find TRUE/FALSE columns
  m1 <- c(which(d[1,]=="False"),which(d[1,]=="True"))
  d[,m1] <- (d[,m1]=="True")

  d
}

