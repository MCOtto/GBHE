#' Sort observer initials by last name first
#'
#' @param str Observer initials
#'
#' @return
#' @export
#'
#' @examples
#' LastFirst("DDA")
#' LastFirst("HS")
#' LastFirst("MCO")
LastFirst <- function(str) {
  nc <- nchar(str)
  paste(substr(str, nc, nc), substr(str, 1, nc - 1), sep = "")
}
