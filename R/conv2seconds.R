#' Convert Time String to Numeric
#'
#' @param strTime a string with time
#'
#' @returns a numeric value in units of seconds
#' @author Thomas Gredig
#'
#' @examples
#' conv2seconds("00:35:40.1816298")
#' conv2seconds("00:35:40.1816298") - conv2seconds("00:36:40.1816298")
#' conv2seconds("1.19:07:06.5180408")
#'
#' @export
conv2seconds <- function(strTime) {
  #strTime = "00:35:40.1816298"
  num = strsplit(strTime,":")[[1]]
  if (grepl('\\.',num[1])) {
    num1 = as.numeric(strsplit(num[1],"\\.")[[1]])
    num[1]=num1[1]*24+num1[2]
  }
  num = as.numeric(num)
  num[1]*3600 + num[2]*60 + num[3]
}
