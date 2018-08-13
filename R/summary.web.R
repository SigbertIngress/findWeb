#' summary.web
#'
#' Prints a summary of a web.
#'
#' @param object a web
#' @param ... further parameters (unused)
#'
#' @export
#'
#' @examples
#' xy <- cbind(runif(30), runif(30))
#' g  <- fishbone(8)
#' summary(g)
#' gf <- web(g, xy)
#' summary(gf)
summary.web <- function(object, ...) {
  cat(sprintf("Vertices       : %.0f\n", nrow(object$vertices)))
  cat(sprintf("Vertex size    : %.0f\n", max(object$map, na.rm=TRUE)))
  cat(sprintf("Edge size      : %.0f\n", nrow(object$edges)))  
  cat(sprintf("Face size      : %.0f\n", nrow(object$faces)))
  cat(sprintf("Intersection(s): %.0f\n", sum(object$error$Intersection)/2))
  cat(sprintf("Vertex diff    : %.0f\n", sum(object$error$VertDiff)))
}