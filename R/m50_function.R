#basic function to multiply number by 50

#' Multiply by 50
#'
#' @param x Value that you want to multiply
#'
#' @returns x multiplied by 50
#' @export
#'
#' @examples
#' m50(10)
#' [1] 500
m50 <- function(x) {

  if (!is.numeric(x)) {
    stop("x must be numeric")
  }
  x * 50

}
