#' Basic Information from Thermal Evaporator Deposition
#'
#' Imports data from the CSV log file of an Angstrom Engineering
#' Thermal Evaporator. It extracts information during the
#' deposition; i.e. while the shutter is open. It returns a condensed
#' version of the deposition parameters.
#'
#' @param filename full path of ATE Log file
#' @param verbose set to TRUE to get additional information
#'
#' @seealso [ATE.import()]
#'
#' @examples
#' fileName = ATE.sampleFiles("csv")[1]
#' d = ATE.info(fileName,TRUE)
#' head(d)
#'
#' @importFrom utils tail
#'
#' @author Thomas Gredig
#'
#' @return list with information during the deposition that includes the thickness,
#'       the deposition time in seconds, the starting date, the substrate heater temperature,
#'       the material deposition temperature, tooling factor, base pressure, the pressure at start of the deposition,
#'       maximum pressure, and the material name
#'
#'
#' @export
ATE.info <- function(filename, verbose=FALSE) {
  if (verbose) cat("Loading ATE file:",filename,"\n")
  d <- ATE.import(filename)

  m1 <- which(d$SubstrateShutterOpen==TRUE)
  if (length(m1)==0) {
    if (verbose) cat("Shutter was not opened.\n")
    return(list())
  }
  d1 <- d[m1,]

  if (length(grep('Radak.3', names(d1)) ) >0) {
    thickness = tail(d1$Radak.3.ActualThickness,1) - d1$Radak.3.ActualThickness[2]
    temp.mat = mean(d1$Radak.3.ActualTemperature)
    mat.name = d1$Radak.3.MaterialName[1]
  } else {
    thickness = tail(d1$Radak.1.ActualThickness,1) - d1$Radak.1.ActualThickness[2]
    temp.mat = mean(d1$Radak.1.ActualTemperature)
    mat.name = d1$Radak.1.MaterialName[1]
  }

  if (length(grep('Sensor.2', names(d1)) ) >0) {
    tooling = d1$Physical.Sensor.2.ToolingFactor[1]
  } else {
    tooling = d1$Physical.Sensor.1.ToolingFactor[1]
  }

  if (is.na(temp.mat)) temp.mat = NA
  if (is.null(mat.name)) mat.name = "?"
  if (length(thickness)==0) thickness = NA

  dInfo <- list(
    filename = basename(filename),
    thickness = thickness,
    time.dep = conv2seconds(tail(d1$Elapsed.Time,1)) - conv2seconds(d1$Elapsed.Time[1]),
    date.dep = d1$Date[1],
    T.dep = mean(d1$Substrate.Heater.ActualTemperature),
    T.material = temp.mat,
    tooling.factor = tooling,
    pressure.base = min(d$ChamberPressure),
    pressure.start = d1$ChamberPressure[1],
    pressure.max = max(d1$ChamberPressure),
    material = mat.name
  )

  dInfo
}
