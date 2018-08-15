#' swapi
#'
#' Swaps interactively the given structure between portals for web optimization by hand. 
#' To finish press Esc or choose two portals which do no belong to the given linking structure.
#
#' @param g a web
#' @param ... further parameters for plotting, see \code{\link{plot.web}}
#'
#' @return the modified web
#' @export
#'
#' @examples
#' \dontrun
#' xy <- cbind(runif(20), runif(20))
#' g  <- fishbone(8)
#' gf <- web(g, xy)
#' gs <- swapi(gf)
#' summary(gs)
#' }
swapi <- function(g, ...) {
  repeat {
    plot(g, ...)
    p <- identify(x=g$vertices, n=2, plot=FALSE)
    if(all(is.na(g$map[p])))  break
    g <- swap(g, p[1], p[2])
  }
  g$error <- evaluateC(g)
  g
}