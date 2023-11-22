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

  dInfo <- list(
    filename = basename(filename),
    thickness = tail(d1$Radak.3.ActualThickness,1) - d1$Radak.3.ActualThickness[2],
    time.dep = conv2seconds(tail(d1$Elapsed.Time,1)) - conv2seconds(d1$Elapsed.Time[1]),
    date.dep = d1$Date[1],
    T.dep = mean(d1$Substrate.Heater.ActualTemperature),
    T.material = mean(d1$Radak.3.ActualTemperature),
    tooling.factor = d1$Physical.Sensor.2.ToolingFactor[1],
    pressure.base = min(d$ChamberPressure),
    pressure.start = d1$ChamberPressure[1],
    pressure.max = max(d1$ChamberPressure),
    material = d1$Radak.3.MaterialName[1]
  )

  dInfo
}
