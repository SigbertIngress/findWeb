#' homogeneous
#'
#' Creates a homogeneous mutlilayer field (see \href{https://www.youtube.com/watch?v=OvxD-iMgVPM}{Michael Hartley's three part videos}).
#'
#' @param n numeric: number of layers
#'
#' @return a web
#' @export
#'
#' @examples
#' g <- homogeneous(2) # the most used homogeneous field
#' plot(g)
homogeneous <- function(n) {
  subdivide <- function(l, tri, depth) {
    p   <- nrow(l$v)+1
    l$v <- rbind(l$v, colMeans(l$v[tri,]))
    l$e <- rbind(l$e, cbind(tri, rep(p,3)))
    if(depth>1) {
      for (i in 1:3) {
        l <- subdivide(l, c(tri[-i],p), depth-1)
      }
    }
    l
  }
  #
  if (n<1) stop("n must be positive")
  a   <- 2*pi*(0:2)/3
  ret <- list(v=cbind(cos(a), sin(a)), e=cbind(c(1,1,2), c(2,3,3)))
  ret <- subdivide (ret, 1:3, n-1)
  vertices <- ret$v
  edges    <- normalize(ret$e)
  faces    <- edges2faces(edges)
  ret <- list(vertices=vertices, map=1:nrow(vertices), edges=edges, faces=faces, pts=pointsInTrianglesC(vertices, faces))
  class(ret) <- 'web'
  ret$error  <- evaluateC(ret)
  ret
}
