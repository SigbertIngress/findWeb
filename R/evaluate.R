#' evaluate
#'
#' For given web and a portal set it computes the number of intersections of links and the number of portals in each field.
#' These two numbers give the error of the web. 
#'
#' @param g a web 
#'
#' @return an error value
#' @export
#'
#' @examples
#' xy <- cbind(runif(30), runif(30))
#' g  <- fishbone(8)
#' evaluate(g)
#' gf <- web(g, xy)
#' evaluate(gf)
evaluate <- function(g) {
  if (is.null(g$error)) g$error <- evaluateC(g)
  sum(g$error$Intersection)/2+sum(g$error$VertDiff)
}