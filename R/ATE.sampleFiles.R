#' Sample ATE files
#'
#' Returns a list of sample thermal evaporator files.
#'
#' @param PATR pattern to limit the files
#'
#' @examples
#' ATE.sampleFiles()
#' ATE.sampleFiles('_Status')
#'
#' @export
ATE.sampleFiles <- function(PATR="*") {
  p = system.file("extdata", package='angstromATE')
  file.path(p, dir(p, pattern=PATR))
}
