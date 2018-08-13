#' swap
#'
#' Swaps the given structure between portals. Mainly for use in \link{optimizeWeb}.
#
#' @param g a web
#' @param i numeric: index of first portal to be swapped
#' @param j numeric: index of second portal to be swapped
#'
#' @return the modified web
#' @export
#'
#' @examples
#' par(mfrow=c(1,2))
#' g <- fishbone(8)
#' plot(g)
#' gs <- swap(g, 1, 3)
#' plot(gs)
swap <- function(g, i, j) {
  tmp <- g$map[i]
  g$map[i] <- g$map[j]
  g$map[j] <- tmp
  g$error <- evaluateC(g)
  g
}