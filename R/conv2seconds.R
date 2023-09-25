#' Convert Time String to Numeric
#'
#' @param strTime a string with time
#'
#' @returns a numeric value in units of seconds
#' @author Thomas Gredig
#'
#' @examples
#' conv2seconds("00:35:40.1816298")
#'
#' @export
conv2seconds <- function(strTime) {
  #strTime = "00:35:40.1816298"
  as.numeric(substr(strTime, 7,50)) +
    as.numeric(substr(strTime, 1,2))*3600 +
    as.numeric(substr(strTime, 4,5))*60
}
