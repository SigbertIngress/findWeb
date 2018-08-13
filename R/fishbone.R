#' fishbone
#'
#' Creates a fishbone structure. Note that the base portals have the numbers one and two.
#'
#' @param n numeric: number of links at base portals 
#'
#' @return a fishbone web
#' @export
#'
#' @examples
#' g <- fishbone(8)
#' plot(g)
fishbone <- function(n) {
  n1 <- n-1
  vertices <- scale(cbind(c(-1,1,rep(0, n-1)), 
                          c(0,0,(1:n1/n))
  ))
  edges    <- cbind(c(rep(1, n), rep(2, n1), 3:n), 
                    c(2:(n+1), 3:(n+1), 4:(n+1)))
  edges    <- normalize(edges)
  faces    <- edges2faces(edges)
  ret <- list(vertices=vertices, map=1:nrow(vertices), edges=edges, faces=faces, pts=pointsInTrianglesC(vertices, faces))
  class(ret) <- 'web'
  ret$error  <- evaluateC(ret)
  ret
}