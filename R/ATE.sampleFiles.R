#' Sample ATE file list
#'
#' Returns a list of sample thermal evaporator log files, mostly for testing.
#'
#' @param filePattern pattern to limit the files
#'
#' @returns list of sample data files with log information
#'
#' @examples
#' ATE.sampleFiles()
#' ATE.sampleFiles('_Status')
#'
#' @export
ATE.sampleFiles <- function(filePattern="*") {
  p = system.file("extdata", package='angstromATE')
  file.path(p, dir(p, pattern=filePattern))
}
