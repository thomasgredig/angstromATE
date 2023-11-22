#' Basic Information from Deposition
#'
#' @param filename full path of ATE Log file
#' @seealso [ATE.import()]
#'
#' @importFrom utils tail
#'
#' @author Thomas Gredig
#'
#' @export
ATE.info <- function(filename) {
  d <- ATE.import(filename)

  m1 <- which(d$SubstrateShutterOpen==TRUE)
  if (length(m1)==0) return(list())
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
