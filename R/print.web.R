#' print.web
#'
#' @param x a web
#' @param ... further parameters (unused)
#'
#' @export
#' 
#' @examples
#' xy <- cbind(runif(30), runif(30))
#' g  <- fishbone(8)
#' g
#' gf <- web(g, xy)
#' gf
print.web <- function(x, ...) {
  str(x)
}