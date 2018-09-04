#' cobweb
#'
#' Creates a cobweb structure. Note that in each leg the portal number increase by three.
#'
#' @param n numeric: number of portals on each leg at the cobweb
#' @param r numeric: distance between nested triangles, the quotient between incirle and cricumcircle of an equilateral triangle (default: 0.5)
#' @param central logical: create central portal (default: FALSE)
#'
#' @return a cobweb
#' @export
#'
#' @examples
#' g <- cobweb(4)
#' plot(g)
cobweb <- function(n, r=0.5, central=FALSE) {
  if (r>0.5) stop("r must be smaller than 0.5")
  vertices <- matrix(0, ncol=2, nrow=0)
  edges    <- matrix(0, ncol=2, nrow=0)
  p <- cbind(sin(2*pi/3*(0:2)), cos(2*pi/3*(0:2)))
  for (i in 1:n) {
    vertices <- rbind(vertices, (r^i)*p)
    ind <- 3*(i-1)+(1:3)  
    if (i>1) {
      edges <- rbind(edges, c(ind[1], ind[1]-3),
                     c(ind[2], ind[2]-3),
                     c(ind[3], ind[3]-3))
      edges <- rbind(edges, c(ind[2], ind[1]-3),
                     c(ind[3], ind[1]-3),
                     c(ind[2], ind[3]-3))
    }
    edges <- rbind(edges, ind[-1], ind[-2], ind[-3])
  }
  if (central) {
    vertices <- rbind(vertices, c(0,0))
    p        <- 3*n+1
    edges    <- rbind(edges, cbind(rep(p, 3), p-(1:3)))
  }
  edges <- normalize(edges)
  faces <- edges2faces(edges)
  pts   <- pointsInTrianglesC(vertices, faces)
  ret <- list(vertices=scale(vertices), map=1:nrow(vertices), edges=edges, faces=faces,pts=pts)
  class(ret) <- 'web'
  ret$error  <- evaluateC(ret)
  ret  
}
